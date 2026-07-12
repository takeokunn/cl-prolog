;;;; CPS proof search core.
;;;;
;;;; Proof search is written in continuation-passing style: every prover
;;;; receives an EMIT continuation and calls it once per solution
;;;; environment.  Solutions therefore stream to the caller; nothing in the
;;;; engine accumulates result lists.
;;;;
;;;; Cut (!) is implemented with CATCH/THROW: each predicate invocation
;;;; establishes a fresh catch tag carried through the proof state, and !
;;;; throws to the tag of the clause it appears in (see prover.lisp).

(in-package #:cl-prolog)

(defparameter *max-prolog-depth* nil
  "Default rule-resolution depth bound; NIL means unbounded search.")

;;; Prolog exception data

(define-condition prolog-exception (error)
  ((term :initarg :term :reader prolog-exception-term)
   (environment :initarg :environment :reader %prolog-exception-environment))
  (:report (lambda (condition stream)
             (format stream "Uncaught Prolog exception: ~S."
                     (prolog-exception-term condition))))
  (:documentation "A thrown term or ISO error term raised during Prolog execution."))

(define-condition prolog-runtime-error (prolog-exception) ()
  (:documentation "Base condition for engine-generated ISO Prolog errors."))

(define-condition prolog-instantiation-error (prolog-runtime-error) ())
(define-condition prolog-type-error (prolog-runtime-error) ())
(define-condition prolog-domain-error (prolog-runtime-error) ())
(define-condition prolog-permission-error (prolog-runtime-error) ())
(define-condition prolog-existence-error (prolog-runtime-error) ())
(define-condition prolog-evaluation-error (prolog-runtime-error) ())
(define-condition prolog-resource-error (prolog-runtime-error) ())
(define-condition prolog-syntax-error (prolog-runtime-error) ())

(define-condition invalid-max-depth-error (error)
  ((value :initarg :value :reader invalid-max-depth-error-value))
  (:report (lambda (condition stream)
             (format stream ":MAX-DEPTH must be NIL or a non-negative integer, got ~S."
                     (invalid-max-depth-error-value condition))))
  (:documentation "Signalled when a query receives an invalid :MAX-DEPTH option."))

(define-condition prolog-halt (serious-condition)
  ((code :initarg :code :reader prolog-halt-code))
  (:report (lambda (condition stream)
             (format stream "Prolog requested halt with exit code ~D."
                     (prolog-halt-code condition))))
  (:documentation "Raised by halt/0 and halt/1; embedders decide how to exit.

Deliberately not a PROLOG-EXCEPTION: catch/3 must not intercept it."))

(define-condition prolog-depth-limit-exceeded (prolog-resource-error)
  ((goal :initarg :goal :reader prolog-depth-limit-exceeded-goal))
  (:report (lambda (condition stream)
             (format stream "Prolog rule-resolution depth limit reached while proving ~S."
                     (prolog-depth-limit-exceeded-goal condition))))
  (:documentation "Signalled when proof search would exceed an explicit rule depth bound."))

(defun %validate-max-depth (value)
  "Return VALUE when it is a valid rule depth bound, otherwise signal an error."
  (unless (typep value '(or null (integer 0 *)))
    (error 'invalid-max-depth-error :value value))
  value)

(define-condition invalid-goal-error (prolog-type-error)
  ((goal :initarg :goal :reader invalid-goal-error-goal)
   (reason :initarg :reason :reader invalid-goal-error-reason))
  (:report (lambda (condition stream)
             (format stream "Invalid Prolog goal ~S: ~A."
                     (invalid-goal-error-goal condition)
                     (invalid-goal-error-reason condition))))
  (:documentation "Signalled when a goal is structurally unusable."))

(declaim (ftype function %iso-atom %iso-term %iso-error-term))

(defun %invalid-goal (goal reason &rest arguments)
  (let ((message (apply #'format nil reason arguments)))
    (error 'invalid-goal-error
           :goal goal
           :reason message
           :term (%iso-error-term (%iso-term "TYPE_ERROR" (%iso-atom "CALLABLE") goal)
                                  (%iso-atom "CALL") message)
           :environment nil)))

;;; Prolog exception construction and control flow

(defun %iso-atom (name)
  "Return the stable Prolog atom for ISO error vocabulary NAME."
  (%prolog-atom-symbol (string-downcase name)))

(defun %iso-term (functor &rest arguments)
  "Construct an ISO exception term without inheriting Common Lisp symbols."
  (cons (%iso-atom functor) arguments))

(defun %iso-error-term (formal operation message)
  "Wrap FORMAL in the ISO error/2 context used by public engine failures."
  (%iso-term "ERROR" formal (%iso-term "CONTEXT" operation message)))

(defun %raise-iso-error (condition-type formal environment operation message)
  "Raise CONDITION-TYPE carrying a catchable ISO error term."
  (error condition-type
         :term (%iso-error-term formal operation message)
         :environment environment))

(defun %raise-instantiation-error (environment operation message)
  (%raise-iso-error 'prolog-instantiation-error
                    (%iso-atom "INSTANTIATION_ERROR")
                    environment operation message))

(defun %raise-type-error (expected culprit environment operation message)
  (%raise-iso-error 'prolog-type-error
                    (%iso-term "TYPE_ERROR" (%iso-atom expected) culprit)
                    environment operation message))

(defun %raise-domain-error (domain culprit environment operation message)
  (%raise-iso-error 'prolog-domain-error
                    (%iso-term "DOMAIN_ERROR" (%iso-atom domain) culprit)
                    environment operation message))

(defun %raise-permission-error (operation permission-type culprit environment context message)
  (%raise-iso-error 'prolog-permission-error
                    (%iso-term "PERMISSION_ERROR" (%iso-atom operation)
                               (%iso-atom permission-type) culprit)
                    environment context message))

(defun %raise-existence-error (object-type culprit environment operation message)
  (%raise-iso-error 'prolog-existence-error
                    (%iso-term "EXISTENCE_ERROR" (%iso-atom object-type) culprit)
                    environment operation message))

(defun %raise-evaluation-error (reason environment operation message)
  (%raise-iso-error 'prolog-evaluation-error
                    (%iso-term "EVALUATION_ERROR" (%iso-atom reason))
                    environment operation message))

(defun %raise-resource-error (resource environment operation message &key condition-type goal)
  (let ((term (%iso-error-term (%iso-term "RESOURCE_ERROR" (%iso-atom resource))
                               operation message)))
    (if condition-type
        (error condition-type :term term :environment environment :goal goal)
        (error 'prolog-resource-error :term term :environment environment))))

(defun %raise-syntax-error (condition environment operation)
  "Raise parser CONDITION as a catchable ISO syntax_error/1 term."
  (let ((description (prolog-parse-error-description condition)))
    (%raise-iso-error
     'prolog-syntax-error
     (%iso-term "SYNTAX_ERROR"
                (%prolog-atom-symbol description :preserve-case t))
     environment operation description)))

(defun %raise-prolog-exception (term environment)
  "Raise TERM together with the binding environment active at THROW/1."
  (if (logic-var-p term)
      (%raise-instantiation-error environment (%iso-atom "THROW")
                                  "throw/1 requires an instantiated term")
      (error 'prolog-exception :term term :environment environment)))

;;; Builtin goal dispatch

(defvar *fixed-builtin-solvers* (make-hash-table :test #'equal))
(defvar *variadic-builtin-solvers* (make-hash-table :test #'eq))

(defun %goal-solver (predicate arity)
  "Return the builtin solver registered for PREDICATE/ARITY, if any."
  (or (gethash (cons predicate arity) *fixed-builtin-solvers*)
      (let ((entry (gethash predicate *variadic-builtin-solvers*)))
        (when (and entry (>= arity (car entry)))
          (cdr entry)))))

(defvar *builtin-predicate-indicators* '())

(defun %register-builtin-predicate! (predicate arity)
  "Register the canonical indicator exposed by CURRENT-PREDICATE/1."
  (pushnew (list '/ predicate arity) *builtin-predicate-indicators*
           :test #'equal)
  predicate)

(defun %register-builtin-solver! (predicate minimum maximum solver)
  "Register SOLVER for PREDICATE, replacing a definition loaded previously."
  (if maximum
      (setf (gethash (cons predicate maximum) *fixed-builtin-solvers*) solver)
      (setf (gethash predicate *variadic-builtin-solvers*)
            (cons minimum solver)))
  (%register-builtin-predicate! predicate minimum))

(defun %builtin-predicate-indicators ()
  "Return a detached snapshot of builtin predicate indicators."
  (reverse (copy-list *builtin-predicate-indicators*)))

(defun %argument-list-arity (argument-list)
  "Return (VALUES MINIMUM MAXIMUM) arity for ARGUMENT-LIST; MAXIMUM is NIL when variadic."
  (let ((required (position '&rest argument-list)))
    (if required
        (values required nil)
        (values (length argument-list) (length argument-list)))))

(defmacro define-builtin ((name &rest argument-list) (rulebase environment depth emit)
                          &body body)
  "Define builtin solvers for goals of shape (NAME . ARGUMENT-LIST).

NAME may also be a list of head symbols sharing one solver.  ARGUMENT-LIST
is an ordinary lambda list (only required parameters and &REST are
supported).  Fixed-arity builtins dispatch on their exact predicate indicator;
variadic builtins dispatch only at or above their required arity.
BODY must call EMIT with one extended environment per solution."
  (multiple-value-bind (minimum maximum)
      (%argument-list-arity argument-list)
    (let ((goal (gensym "GOAL"))
          (names (if (listp name) name (list name))))
      `(progn
         ,@(mapcar
            (lambda (builtin-name)
              `(eval-when (:load-toplevel :execute)
                 (%register-builtin-solver!
                  ',builtin-name ,minimum ,maximum
                  (lambda (,goal ,rulebase ,environment ,depth ,emit)
                    (declare (ignorable ,rulebase ,environment ,depth ,emit))
                    ;; Dispatch guarantees the arity matches: fixed builtins
                    ;; are keyed by their exact indicator, variadic ones only
                    ;; receive goals at or above their required arity.
                    (destructuring-bind ,argument-list (rest ,goal)
                      ,@body)))))
            names)
         ',(first names)))))

;;; Foreign predicate dispatch

(defvar *foreign-predicate-indicators* '())

(defun %foreign-predicate-indicators ()
  "Return a detached snapshot of registered foreign predicate indicators."
  (reverse (copy-list *foreign-predicate-indicators*)))

(defgeneric %foreign-goal-solver (predicate arity)
  (:documentation
   "Return the CPS foreign solver registered for PREDICATE/ARITY."))

(defmethod %foreign-goal-solver (predicate arity)
  (declare (cl:ignore predicate arity))
  nil)

(defmacro define-foreign-predicate ((name &rest argument-list)
                                    (rulebase environment depth emit)
                                    &body body)
  "Define the authoritative foreign solver for the exact predicate NAME/ARITY.

BODY must call EMIT with one extended environment per solution.  Calling EMIT
zero times fails; calling it repeatedly produces multiple solutions."
  (when (find-if (lambda (parameter)
                   (member parameter lambda-list-keywords))
                 argument-list)
    (error "Foreign predicates require a fixed argument list: ~S"
           argument-list))
  (let ((goal (gensym "GOAL"))
        (arity (length argument-list)))
    `(progn
       (eval-when (:load-toplevel :execute)
         (pushnew (list '/ ',name ,arity) *foreign-predicate-indicators*
                  :test #'equal))
       (defmethod %foreign-goal-solver ((predicate (eql ',name))
                                       (arity (eql ,arity)))
         (declare (cl:ignore predicate arity))
         (lambda (,goal ,rulebase ,environment ,depth ,emit)
           (declare (ignorable ,rulebase ,environment ,depth ,emit))
           (destructuring-bind ,argument-list (rest ,goal)
             ,@body))))))
