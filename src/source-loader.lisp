;;;; Transactional Prolog source loading and directive evaluation.

(in-package #:cl-prolog)

(defvar *current-prolog-source-directory* nil)
(defvar *current-prolog-source* nil)
(defvar *current-prolog-source-record* nil)

(define-condition prolog-source-not-found (file-error) ())

(defun %resolve-prolog-source-pathname (pathname)
  (merge-pathnames pathname
                   (or *current-prolog-source-directory*
                       *default-pathname-defaults*)))

(defun %canonical-prolog-source-pathname (pathname)
  "Return PATHNAME's canonical existing identity or signal a source error."
  (let ((resolved (%resolve-prolog-source-pathname pathname)))
    (handler-case
        (truename resolved)
      (file-error ()
        (error 'prolog-source-not-found :pathname resolved)))))

(defun %call-with-prolog-source-stream (source function)
  (etypecase source
    (string
     (with-input-from-string (stream source)
       (funcall function stream)))
    (stream (funcall function source))
    (pathname
     (let* ((resolved (%resolve-prolog-source-pathname source))
            (directory (make-pathname :name nil :type nil :version nil
                                      :defaults resolved)))
       (with-open-file (stream resolved :direction :input
                                       :if-does-not-exist nil)
         (unless stream
           (error 'prolog-source-not-found :pathname resolved))
         (let ((*current-prolog-source-directory* directory))
           (funcall function stream)))))))

(defun %source-file-pathnames (term environment operation)
  "Resolve TERM, an atom or proper list of atoms, to source pathnames."
  (labels ((source-pathname (value)
             (let ((resolved (logic-substitute value environment)))
               (when (logic-var-p resolved)
                 (%raise-instantiation-error environment operation
                                             "Source must be instantiated"))
               (pathname (%io-pathname resolved environment operation))))
           (source-list (value original)
             (let ((resolved (logic-substitute value environment)))
               (cond
                 ((logic-var-p resolved)
                  (%raise-instantiation-error environment operation
                                              "Source list must be instantiated"))
                 ((null resolved) '())
                 ((consp resolved)
                  (cons (source-pathname (car resolved))
                        (source-list (cdr resolved) original)))
                 (t
                  (%raise-type-error "LIST" original environment operation
                                     "Source must be a proper list of atoms"))))))
    (let ((value (logic-substitute term environment)))
      (cond
        ((logic-var-p value)
         (%raise-instantiation-error environment operation
                                     "Source must be instantiated"))
        ((null value) '())
        ((symbolp value) (list (source-pathname value)))
        ((consp value) (source-list value value))
        (t
         (%raise-type-error "ATOM" value environment operation
                            "Source must be an atom or proper list of atoms"))))))

(defmacro with-prolog-source-errors ((environment operation) &body body)
  "Translate source loading failures into operation-specific ISO errors."
  `(handler-case
       (progn ,@body)
     (prolog-source-not-found (condition)
       (%raise-existence-error
        "SOURCE_SINK"
        (%prolog-atom-symbol
         (namestring (file-error-pathname condition))
         :preserve-case t)
        ,environment ,operation
        "Source file does not exist"))
     (prolog-parse-error (condition)
       (%raise-syntax-error condition ,environment ,operation))))

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
    (pathnames rulebase &optional (initializations (list '()))
                          &key (if-loaded :reload))
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
