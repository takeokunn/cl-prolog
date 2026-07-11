;;;; Prolog module namespace data and deterministic name resolution.

(in-package #:cl-prolog)

(defconstant +default-prolog-module+ 'user)

(defstruct (prolog-module
            (:constructor %make-prolog-module (name exports imports)))
  "One module declaration.  Predicate indicators are represented as (NAME / ARITY)."
  (name +default-prolog-module+ :type symbol :read-only t)
  (exports (make-hash-table :test #'equal) :type hash-table :read-only t)
  (imports (make-hash-table :test #'equal) :type hash-table :read-only t))

(defstruct (module-registry (:constructor %make-module-registry (modules)))
  "Mutable namespace registry owned by one rulebase."
  (modules (make-hash-table :test #'eq) :type hash-table :read-only t))

(define-condition prolog-module-error (error)
  ((operation :initarg :operation :reader prolog-module-error-operation)
   (detail :initarg :detail :reader prolog-module-error-detail))
  (:report (lambda (condition stream)
             (format stream "Cannot ~A: ~A."
                     (prolog-module-error-operation condition)
                     (prolog-module-error-detail condition)))))

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
      ((and (symbolp first) (eq second '/)
            (typep third '(integer 0)))
       (cons first third))
      ((and (eq first '/) (symbolp second)
            (typep third '(integer 0)))
       (cons second third))
      (t (%module-error operation "invalid predicate indicator ~S" indicator)))))

(defun make-module-registry ()
  "Create a registry containing the implicit USER module."
  (let ((registry (%make-module-registry (make-hash-table :test #'eq))))
    (setf (gethash +default-prolog-module+ (module-registry-modules registry))
          (%make-prolog-module +default-prolog-module+
                               (make-hash-table :test #'equal)
                               (make-hash-table :test #'equal)))
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
  (let ((export-table (make-hash-table :test #'equal)))
    (dolist (indicator exports)
      (let ((key (%predicate-indicator-key indicator "declare module")))
        (when (gethash key export-table)
          (%module-error "declare module" "duplicate export ~S" indicator))
        (setf (gethash key export-table) t)))
    (setf (gethash name (module-registry-modules registry))
          (%make-prolog-module name export-table
                               (make-hash-table :test #'equal)))))

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
  (let ((copy (%make-module-registry (make-hash-table :test #'eq))))
    (maphash
     (lambda (name module)
       (let ((exports (make-hash-table :test #'equal))
             (imports (make-hash-table :test #'equal)))
         (maphash (lambda (key value) (setf (gethash key exports) value))
                  (prolog-module-exports module))
         (maphash (lambda (key value) (setf (gethash key imports) value))
                  (prolog-module-imports module))
         (setf (gethash name (module-registry-modules copy))
               (%make-prolog-module name exports imports))))
     (module-registry-modules registry))
    copy))
