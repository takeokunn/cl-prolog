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

(defun %read-source-term (stream operator-table)
  (let* ((source (%read-prolog-term-source stream))
         (parser (%parser (%tokenize-prolog source operator-table) operator-table)))
    (if (eq :eof (%token-kind (%current-token parser)))
        (values nil nil)
        (let ((term (%parse-expression parser (make-hash-table :test #'equal) 0)))
          (%expect-token parser :operator ".")
          (%expect-token parser :eof)
          (values term t)))))

(defun %predicate-indicator-values (indicator directive)
  (let ((key (%predicate-indicator-key indicator directive)))
    (values (car key) (cdr key))))

(defun %operator-specifier-keyword (specifier)
  (unless (symbolp specifier)
    (error "Operator specifier must be an atom, got ~S." specifier))
  (let ((keyword (intern (symbol-name specifier) '#:keyword)))
    (unless (%valid-operator-specifier-p keyword)
      (error "Invalid operator specifier ~S." specifier))
    keyword))

(defun %record-source-operator-effect! (effect)
  (when *current-prolog-source-record*
    (push effect (%source-record-operators *current-prolog-source-record*))))

(defun %record-source-predicate-property!
    (module predicate arity property)
  (when *current-prolog-source-record*
    (push (list module predicate arity property)
          (%source-record-predicate-properties
           *current-prolog-source-record*))))

(defun %apply-source-directive! (goal rulebase initializations module)
  (unless (consp goal)
    (error "Unknown Prolog directive ~S." goal))
  (case (first goal)
    (op
     (unless (= (length goal) 4)
       (error "OP directive requires priority, specifier, and name."))
     (destructuring-bind (name priority specifier operator) goal
       (declare (ignore name))
       (let* ((keyword (%operator-specifier-keyword specifier))
              (previous (%operator-table-find
                         (rulebase-operator-table rulebase) operator keyword)))
         (%record-source-operator-effect!
          (list operator keyword
                (and previous
                     (operator-definition-priority (first previous)))
                priority))
         (setf (rulebase-operator-table rulebase)
               (%operator-table-define
                (rulebase-operator-table rulebase)
                operator priority keyword)))))
    (dynamic
     (unless (= (length goal) 2)
       (error "DYNAMIC directive requires one predicate indicator."))
     (multiple-value-bind (predicate arity)
         (%predicate-indicator-values (second goal) 'dynamic)
       (%set-rulebase-predicate-property! rulebase predicate arity :dynamic module)
       (%record-source-predicate-property!
        module predicate arity :dynamic)))
    (use_module
     (unless (member (length goal) '(2 3))
       (error "USE_MODULE directive requires a module and optional imports."))
     (module-registry-import! (rulebase-module-registry rulebase)
                              module (second goal) (third goal)))
    (initialization
     (unless (= (length goal) 2)
       (error "INITIALIZATION directive requires one callable goal."))
     (push (cons module (second goal)) (car initializations)))
    ((consult ensure_loaded)
     (unless (= (length goal) 2)
       (error "~A directive requires one source argument." (first goal)))
     (%load-prolog-source-into-rulebase
      (%source-file-pathnames (second goal) nil (first goal))
      rulebase initializations
      :if-loaded (if (eq (first goal) 'ensure_loaded) :skip :reload)))
    (load_files
     (unless (member (length goal) '(2 3))
       (error "LOAD_FILES directive requires sources and optional options."))
     (%load-prolog-source-into-rulebase
      (%source-file-pathnames (second goal) nil 'load_files)
      rulebase initializations
      :if-loaded (if (= (length goal) 2)
                     :reload
                     (%load-files-if-loaded-policy (third goal) nil 'load_files))))
    ((set_prolog_flag char_conversion)
     (unless (= (length goal) 3)
       (error "~A directive requires two arguments." (first goal)))
     (unless (prolog-succeeds-p rulebase goal)
       (error "Prolog directive failed: ~S." goal)))
    ((discontiguous multifile)
     (unless (rest goal)
       (error "~A directive requires predicate indicators." (first goal)))
     ;; The engine already resolves clauses independently of their textual
     ;; grouping and origin, so the declaration only needs validation.
     (dolist (indicator (rest goal))
       (%predicate-indicator-values indicator (first goal))))
    (include
     (unless (= (length goal) 2)
       (error "INCLUDE directive requires one source argument."))
     (dolist (pathname (%source-file-pathnames (second goal) nil 'include))
       (%call-with-prolog-source-stream
        (%canonical-prolog-source-pathname pathname)
        (lambda (stream)
          (%process-included-source-terms
           stream rulebase initializations module)))))
    (otherwise
     (error "Unknown Prolog directive ~S." goal))))

(defun %process-included-source-terms (stream rulebase initializations module)
  "Splice STREAM's terms into the including source unit under MODULE."
  (loop
    (multiple-value-bind (term present-p)
        (let ((*active-char-conversions*
                (%rulebase-active-char-conversions rulebase)))
          (%read-source-term stream (rulebase-operator-table rulebase)))
      (unless present-p (return))
      (cond
        ((and (consp term) (eq (first term) '?-))
         (error "Queries are not consultable source forms: ~S." term))
        ((and (consp term)
              (eq (first term) (%prolog-symbol ":-"))
              (= (length term) 2))
         (let ((directive (second term)))
           (when (and (consp directive) (eq (first directive) 'module))
             (error "MODULE directives are not allowed in included files."))
           (%apply-source-directive! directive rulebase initializations module)))
        (t
         (%insert-source-clause! rulebase term module))))))

(defun %source-term-clause (term)
  (cond
    ((symbolp term) (make-clause (list term)))
    ((and (consp term) (eq (first term) '-->) (= (length term) 3))
     (%expand-prolog-dcg-clause (second term) (third term)))
    ((and (consp term)
          (eq (first term) (%prolog-symbol ":-"))
          (= (length term) 3))
     (let ((head (second term)))
       (unless (or (symbolp head) (consp head))
         (error "Invalid Prolog clause head ~S." head))
       (make-clause (if (symbolp head) (list head) head)
                    (%body-goals (third term)))))
    ((consp term) (make-clause term))
    (t (error "Invalid consultable Prolog term ~S." term))))

(defun %insert-source-clause! (rulebase term module)
  (let ((clause (%source-term-clause term)))
    (multiple-value-bind (predicate arity)
        (values (first (clause-head clause)) (length (rest (clause-head clause))))
      (module-registry-ensure-definition-allowed
       (rulebase-module-registry rulebase) module predicate arity)
      (unless (%rulebase-predicate-property rulebase predicate arity module)
        (%set-rulebase-predicate-property! rulebase predicate arity :static module)
        (%record-source-predicate-property!
         module predicate arity :static)))
    (rulebase-insert-clause! rulebase clause :module module
                             :source *current-prolog-source*)))

(defun %remove-source-clauses! (rulebase canonical)
  (multiple-value-bind (revision entries) (%rulebase-snapshot rulebase)
    (declare (ignore revision))
    (%rulebase-retract-entries!
     rulebase
     (remove canonical entries :test-not #'equal
                               :key #'%stored-clause-source))))

(defun %remove-source-operators! (rulebase record)
  ;; Effects are pushed during loading, so replaying them restores prior layers.
  (dolist (effect (%source-record-operators record))
    (destructuring-bind (name specifier previous-priority source-priority) effect
      (let ((current (%operator-table-find
                      (rulebase-operator-table rulebase) name specifier)))
        (when (if (zerop source-priority)
                  (null current)
                  (and current
                       (= source-priority
                          (operator-definition-priority (first current)))))
          (setf (rulebase-operator-table rulebase)
                (%operator-table-define
                 (rulebase-operator-table rulebase) name
                 (or previous-priority 0) specifier)))))))

(defun %source-operator-overrides (rulebase record)
  "Return runtime definitions currently shadowing RECORD's latest effects."
  (let ((seen '())
        (overrides '()))
    (dolist (effect (%source-record-operators record) overrides)
      (destructuring-bind (name specifier previous-priority source-priority) effect
        (declare (ignore previous-priority))
        (let ((key (list name specifier)))
          (unless (member key seen :test #'equal)
            (push key seen)
            (let ((current (%operator-table-find
                            (rulebase-operator-table rulebase) name specifier)))
              (when (and current
                         (/= source-priority
                             (operator-definition-priority (first current))))
                (push (first current) overrides)))))))))

(defun %restore-operator-overrides! (rulebase overrides)
  (dolist (definition overrides)
    (setf (rulebase-operator-table rulebase)
          (%operator-table-define
           (rulebase-operator-table rulebase)
           (operator-definition-name definition)
           (operator-definition-priority definition)
           (operator-definition-specifier definition)))))

(defun %remove-source-predicate-properties! (rulebase record)
  (dolist (effect (%source-record-predicate-properties record))
    (destructuring-bind (module predicate arity property) effect
      (when (eq property
                (%rulebase-predicate-property
                 rulebase predicate arity module))
        (%remove-rulebase-predicate-property!
         rulebase predicate arity module)))))

(defun %remove-source-artifacts! (rulebase canonical record)
  (%remove-source-clauses! rulebase canonical)
  (%remove-source-operators! rulebase record)
  (%remove-source-predicate-properties! rulebase record))

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
          ((and (consp term) (eq (first term) '?-))
           (error "Queries are not consultable source forms: ~S." term))
          ((and (consp term)
                (eq (first term) (%prolog-symbol ":-"))
                (= (length term) 2))
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
