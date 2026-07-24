;;;; Prolog module namespace data and deterministic name resolution.

(in-package #:cl-prolog)

(defconstant +default-prolog-module+ 'user)

(defstruct (prolog-module
            (:constructor %make-prolog-module (name exports imports)))
  "One module declaration.  Predicate indicators use the parser AST (/ NAME ARITY)."
  (name +default-prolog-module+ :type symbol :read-only t)
  (exports (%make-equal-hash-table) :type hash-table :read-only t)
  (imports (%make-equal-hash-table) :type hash-table :read-only t))

(defstruct (module-registry (:constructor %make-module-registry (modules)))
  "Mutable namespace registry owned by one rulebase."
  (modules (%make-equal-hash-table) :type hash-table :read-only t))

(defmacro define-contextual-error-condition
    (name (parent) (context-slot context-reader) (reason-slot reason-reader)
     report-format &optional documentation)
  "Define NAME as a PARENT condition carrying two initargs -- CONTEXT-SLOT
(read via CONTEXT-READER) and REASON-SLOT (read via REASON-READER) -- and
reported by formatting REPORT-FORMAT with the context value then the
reason value.  Shared shape for this codebase's \"something went wrong
doing X: reason\" conditions (prolog-module-error, and later
arithmetic-evaluation-error, invalid-goal-error)."
  `(define-condition ,name (,parent)
     ((,context-slot :initarg ,(intern (symbol-name context-slot) :keyword)
                     :reader ,context-reader)
      (,reason-slot :initarg ,(intern (symbol-name reason-slot) :keyword)
                    :reader ,reason-reader))
     (:report (lambda (condition stream)
                (format stream ,report-format
                        (,context-reader condition)
                        (,reason-reader condition))))
     ,@(when documentation `((:documentation ,documentation)))))

(define-contextual-error-condition prolog-module-error (error)
  (operation prolog-module-error-operation)
  (detail prolog-module-error-detail)
  "Cannot ~A: ~A.")

(defun %module-error (operation control &rest arguments)
  (error 'prolog-module-error
         :operation operation
         :detail (apply #'format nil control arguments)))

(defun %predicate-indicator-key (indicator operation)
  "Validate INDICATOR and return its canonical cons key."
  (destructuring-bind (first second third)
      (if (and (consp indicator)
               (consp (rest indicator))
               (consp (rest (rest indicator)))
               (null (rest (rest (rest indicator)))))
          indicator
          (%module-error operation "invalid predicate indicator ~S" indicator))
    (cond
      ((and (eq first '/) (symbolp second)
            (typep third '(integer 0)))
       (cons second third))
      (t (%module-error operation "invalid predicate indicator ~S" indicator)))))

(defun make-module-registry ()
  "Create a registry containing the implicit USER module."
  (let ((registry (%make-module-registry (%make-equal-hash-table))))
    (setf (gethash +default-prolog-module+ (module-registry-modules registry))
          (%make-prolog-module +default-prolog-module+
                               (%make-equal-hash-table)
                               (%make-equal-hash-table)))
    registry))

(defun %find-prolog-module (registry name operation)
  (unless (symbolp name)
    (%module-error operation "module name must be an atom, got ~S" name))
  (or (gethash name (module-registry-modules registry))
      (%module-error operation "unknown module ~S" name)))

(defun module-registry-declare! (registry name exports)
  "Declare NAME with exactly EXPORTS; redeclaration is an error."
  (unless (symbolp name)
    (%module-error "declare module" "module name must be an atom, got ~S" name))
  (when (gethash name (module-registry-modules registry))
    (%module-error "declare module" "module ~S is already declared" name))
  (let ((export-table (%make-equal-hash-table)))
    (dolist (indicator exports)
      (let ((key (%predicate-indicator-key indicator "declare module")))
        (when (gethash key export-table)
          (%module-error "declare module" "duplicate export ~S" indicator))
        (setf (gethash key export-table) t)))
    (setf (gethash name (module-registry-modules registry))
          (%make-prolog-module name export-table
                               (%make-equal-hash-table)))))

(defun module-registry-exported-p (registry module predicate arity)
  "Return true when MODULE publicly exports PREDICATE/ARITY."
  (not (null (gethash (cons predicate arity)
                      (prolog-module-exports
                       (%find-prolog-module registry module "inspect export"))))))

(defun module-registry-import! (registry importer exporter &optional indicators)
  "Import exported INDICATORS from EXPORTER into IMPORTER.

When INDICATORS is NIL all exports are imported.  Importing two origins under
the same predicate indicator is rejected rather than made order-dependent."
  (let* ((target (%find-prolog-module registry importer "import predicates"))
         (source (%find-prolog-module registry exporter "import predicates"))
         (exports (prolog-module-exports source))
         (keys (if indicators
                   (mapcar (lambda (indicator)
                             (%predicate-indicator-key indicator "import predicates"))
                           indicators)
                   (loop for key being the hash-keys of exports collect key))))
    (dolist (key keys)
      (unless (gethash key exports)
        (%module-error "import predicates" "~S does not export ~S/~D"
                       exporter (car key) (cdr key)))
      (multiple-value-bind (origin present-p)
          (gethash key (prolog-module-imports target))
        (when (and present-p (not (eq origin exporter)))
          (%module-error "import predicates"
                         "~S/~D is already imported from ~S"
                         (car key) (cdr key) origin))
        (setf (gethash key (prolog-module-imports target)) exporter)))
    target))

(defun module-registry-ensure-definition-allowed (registry module predicate arity)
  "Reject a local definition that would redefine an imported predicate."
  (let* ((namespace (%find-prolog-module registry module "define predicate"))
         (origin (gethash (cons predicate arity)
                          (prolog-module-imports namespace))))
    (when origin
      (%module-error "define predicate"
                     "~S/~D is imported from ~S" predicate arity origin))
    t))

(defun module-registry-validate-exports (registry module local-p)
  "Reject exported indicators that MODULE does not define.

This is intentionally a finalization operation: module/2 may precede its
definitions in source order."
  (let ((namespace (%find-prolog-module registry module "validate exports")))
    (maphash
     (lambda (key exported-p)
       (declare (ignore exported-p))
       (unless (funcall local-p module (car key) (cdr key))
         (%module-error "validate exports"
                        "module ~S does not define exported predicate ~S/~D"
                        module (car key) (cdr key))))
     (prolog-module-exports namespace))
    t))

(defun module-registry-resolve (registry caller predicate arity local-p)
  "Resolve an unqualified predicate and return its defining module.

LOCAL-P is called with MODULE, PREDICATE, and ARITY.  Local definitions shadow
imports; otherwise the unique import origin is returned."
  (let* ((module (%find-prolog-module registry caller "resolve predicate"))
         (key (cons predicate arity)))
    (cond
      ((funcall local-p caller predicate arity) caller)
      ((gethash key (prolog-module-imports module)))
      (t nil))))

(defun module-registry-resolve-qualified (registry module predicate arity local-p)
  "Resolve MODULE:PREDICATE/ARITY without consulting the caller's imports."
  (%find-prolog-module registry module "resolve qualified predicate")
  (and (funcall local-p module predicate arity) module))

(defun module-registry-copy (registry)
  "Return a transaction-safe deep copy of REGISTRY."
  (let ((copy (%make-module-registry (%make-equal-hash-table))))
    (maphash
     (lambda (name module)
       (setf (gethash name (module-registry-modules copy))
             (%make-prolog-module name
                                   (%copy-hash-table (prolog-module-exports module))
                                   (%copy-hash-table (prolog-module-imports module)))))
     (module-registry-modules registry))
    copy))

(defun %make-equal-hash-table () (make-hash-table :test #'equal))

(defun %copy-hash-table (source)
  "Return a new hash table with SOURCE's test and every key/value pair shallow-copied."
  (let ((copy (make-hash-table :test (hash-table-test source))))
    (maphash (lambda (key value) (setf (gethash key copy) value)) source)
    copy))
