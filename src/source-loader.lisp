;;;; Transaction orchestration and the public consult/load_files/
;;;; ensure_loaded API, built on the stream primitives in source-io.lisp,
;;;; directive evaluation in source-directives.lisp, and the rollback
;;;; support in source-rollback.lisp.

(in-package #:cl-prolog)

(defmacro with-prolog-loading-transaction ((rulebase transaction initializations)
                                           &body body)
  "Evaluate BODY against a copied RULEBASE and publish it on success."
  `(let ((,transaction (%copy-rulebase ,rulebase))
         (,initializations (list '())))
     ,@body
     (%run-source-initializations! ,initializations ,transaction)
     (%replace-rulebase! ,rulebase ,transaction)))

(progn
  (defmacro with-source-loading-builtin ((environment operation emit) &body body)
    "Run a source-loading builtin and publish EMIT after success."
    `(with-prolog-source-errors (,environment ,operation)
       ,@body
       (funcall ,emit ,environment)))
  (defmacro define-source-loading-builtin ((name &rest arguments) operation
                                           &body body)
    "Define a source-loading builtin with shared wrapper logic."
    `(define-builtin (,name ,@arguments) (rulebase environment depth emit)
       (declare (ignore depth))
       (with-source-loading-builtin (environment ,operation emit)
         ,@body))))

(defun %load-prolog-source-transaction (stream rulebase initializations)
  (let ((module +default-prolog-module+)
        (module-declared-p nil)
        (content-seen-p nil))
    (loop
      (multiple-value-bind (term present-p)
          (let ((*active-char-conversions*
                  (%rulebase-active-char-conversions rulebase)))
            (%read-source-term stream (rulebase-operator-table rulebase)))
        (unless present-p (return))
        (cond
          ((%prolog-query-term-p term)
           (error "Queries are not consultable source forms: ~S." term))
          ((%prolog-directive-term-p term)
           (let ((directive (second term)))
             (if (and (consp directive) (eq (first directive) 'module))
                 (progn
                   (when (or content-seen-p module-declared-p)
                     (error "MODULE directive must be the unique first source term."))
                   (unless (= (length directive) 3)
                     (error "MODULE directive requires a name and export list."))
                   (setf module (second directive)
                         module-declared-p t)
                   (module-registry-declare!
                    (rulebase-module-registry rulebase)
                    module (third directive)))
                 (progn
                   (setf content-seen-p t)
                   (%apply-source-directive!
                    directive rulebase initializations module)))))
          (t
           (setf content-seen-p t)
           (%insert-source-clause! rulebase term module)))))
    (when module-declared-p
      (module-registry-validate-exports
       (rulebase-module-registry rulebase) module
       (lambda (candidate predicate arity)
         (not (null (%rulebase-predicate-property
                     rulebase predicate arity candidate))))))
    rulebase))

(defun %run-source-initializations! (initializations rulebase)
  (dolist (initialization (nreverse (car initializations)))
      (unless (prolog-succeeds-p
               rulebase
               (list (%prolog-symbol ":")
                     (car initialization) (cdr initialization)))
        (error "Prolog initialization failed: ~S." (cdr initialization))))
  rulebase)

(defun %load-prolog-pathname-into-rulebase
    (pathname rulebase initializations if-loaded)
  "Load one canonical source, respecting IF-LOADED and breaking load cycles."
  (let* ((canonical (%canonical-prolog-source-pathname pathname))
         (state (%rulebase-source-state rulebase canonical)))
    (unless (or (eq state :loading)
                (and (eq state :loaded) (eq if-loaded :skip)))
      (let* ((old-record (%rulebase-source-record rulebase canonical))
             (operator-overrides
               (and old-record
                    (%source-operator-overrides rulebase old-record)))
            (new-record (%make-source-record :loading)))
        (when old-record
          (%remove-source-artifacts! rulebase canonical old-record))
        (setf (gethash canonical (rulebase-source-registry rulebase)) new-record)
        (let ((*current-prolog-source* canonical)
              (*current-prolog-source-record* new-record))
          (%call-with-prolog-source-stream
           canonical
           (lambda (stream)
             (%load-prolog-source-transaction
              stream rulebase initializations))))
        (%restore-operator-overrides! rulebase operator-overrides))
      (%set-rulebase-source-state! rulebase canonical :loaded)))
  rulebase)

(defun %load-prolog-pathnames-into-rulebase
    (pathnames rulebase initializations &key (if-loaded :reload))
  (dolist (pathname pathnames rulebase)
    (%load-prolog-pathname-into-rulebase
     pathname rulebase initializations if-loaded)))

(defun %load-prolog-source-into-rulebase
    (source rulebase initializations &key (if-loaded :reload))
  "Load SOURCE into RULEBASE, supporting strings, pathnames, and pathname lists."
  (cond
    ((pathnamep source)
     (%load-prolog-pathnames-into-rulebase
      (list source) rulebase initializations :if-loaded if-loaded))
    ((and (listp source) (not (stringp source)))
     ;; Exercised by every load_files/1 call (%source-file-pathnames always
     ;; returns a list); sb-cover under-reports this tail call as partial.
     (%load-prolog-pathnames-into-rulebase
      source rulebase initializations :if-loaded if-loaded))
    (t
     (%call-with-prolog-source-stream
      source
      (lambda (stream)
        (%load-prolog-source-transaction
         stream rulebase initializations))))))

(defun %load-prolog-source-into-rulebase/loaded-once
    (source rulebase &key if-loaded)
  "Load SOURCE into RULEBASE under one transaction with IF-LOADED policy."
  (with-prolog-loading-transaction (rulebase transaction initializations)
    (%load-prolog-source-into-rulebase
     source transaction initializations :if-loaded if-loaded)))

(defun consult-prolog (source &optional (rulebase (make-rulebase)))
  "Consult SOURCE and atomically replace RULEBASE after successful validation."
  (%load-prolog-source-into-rulebase/loaded-once source rulebase))

(defun ensure-prolog-loaded (source &optional (rulebase (make-rulebase)))
  "Load existing pathname SOURCE once, atomically updating RULEBASE."
  (%load-prolog-source-into-rulebase/loaded-once source rulebase
                                                :if-loaded :skip))

(defun %load-files-options (term environment operation)
  "Return a resolved proper option list, reporting ISO list errors."
  (labels ((resolve-list (tail original)
             (let ((resolved (logic-substitute tail environment)))
               (cond
                 ((logic-var-p resolved)
                  (%raise-instantiation-error environment operation
                                              "Load options must be instantiated"))
                 ((null resolved) '())
                 ((consp resolved)
                  (cons (logic-substitute (car resolved) environment)
                        (resolve-list (cdr resolved) original)))
                 (t
                  (%raise-type-error "LIST" original environment operation
                                     "Load options must be a proper list"))))))
    (resolve-list term term)))

(defun %load-files-if-loaded-policy (term environment operation)
  "Validate LOAD_FILES options and return :SKIP for IF(NOT_LOADED)."
  (let ((options (%load-files-options term environment operation)))
    (labels ((contains-variable-p (value)
               (cond
                 ((logic-var-p value) t)
                  ((consp value)
                   (or (contains-variable-p (car value))
                       (contains-variable-p (cdr value))))
                  (t nil)))
             (atom-named-p (value name)
               (and (symbolp value)
                    (string= (symbol-name value) name)))
             (if-not-loaded-option-p (option)
               (and (consp option)
                    (atom-named-p (first option) "IF")
                    (consp (rest option))
                    (null (cddr option))
                    (atom-named-p (second option) "NOT_LOADED")))
             (invalid-option (option message)
               (%raise-domain-error "LOAD_OPTION" option environment operation
                                    message)))
      (dolist (option options)
        (when (contains-variable-p option)
          (%raise-instantiation-error environment operation
                                      "Load options must be instantiated"))
        (unless (if-not-loaded-option-p option)
          (invalid-option option "Expected if(not_loaded)")))
      (unless (= (length options) 1)
        (invalid-option (if options (second options) options)
                        "Exactly one if(not_loaded) option is required"))
      :skip)))

(define-source-loading-builtin (consult source) 'consult
  (consult-prolog (%source-file-pathnames source environment 'consult)
                  rulebase))

(define-source-loading-builtin (load_files sources) 'load_files
  (consult-prolog (%source-file-pathnames sources environment 'load_files)
                  rulebase))

(define-source-loading-builtin (ensure_loaded sources) 'ensure_loaded
  (ensure-prolog-loaded (%source-file-pathnames sources environment
                                                'ensure_loaded)
                        rulebase))

(define-source-loading-builtin (load_files sources options) 'load_files
  (let ((if-loaded (%load-files-if-loaded-policy
                    options environment 'load_files))
        (pathnames (%source-file-pathnames sources environment 'load_files)))
    (with-prolog-loading-transaction (rulebase transaction initializations)
      (%load-prolog-source-into-rulebase
       pathnames transaction initializations :if-loaded if-loaded))))
