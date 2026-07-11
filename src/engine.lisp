;;;; CPS proof search core.
;;;;
;;;; Proof search is written in continuation-passing style: every prover
;;;; receives an EMIT continuation and calls it once per solution
;;;; environment.  Solutions therefore stream to the caller; nothing in the
;;;; engine accumulates result lists.
;;;;
;;;; Cut (!) is implemented with the condition system.  Signalling %CUT
;;;; unwinds to the nearest %WITH-CUT-BARRIER, which is exactly the
;;;; dynamically-closest choice point; re-signalling from a barrier owner
;;;; propagates the cut outward (e.g. from a rule body to the clause list).

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

(define-condition invalid-max-depth-error (error)
  ((value :initarg :value :reader invalid-max-depth-error-value))
  (:report (lambda (condition stream)
             (format stream ":MAX-DEPTH must be NIL or a non-negative integer, got ~S."
                     (invalid-max-depth-error-value condition))))
  (:documentation "Signalled when a query receives an invalid :MAX-DEPTH option."))

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

(defun %raise-prolog-exception (term environment)
  "Raise TERM together with the binding environment active at THROW/1."
  (if (logic-var-p term)
      (%raise-instantiation-error environment (%iso-atom "THROW")
                                  "throw/1 requires an instantiated term")
      (error 'prolog-exception :term term :environment environment)))

;;; Cut control flow

(define-condition %cut (condition) ()
  (:documentation "Control-flow condition signalled by the ! goal."))

(defun %propagate-cut ()
  "Prune the dynamically nearest enclosing choice point."
  (signal '%cut))

(defmacro %with-cut-barrier (&body body)
  "Run BODY as a choice point; return true when a cut pruned it."
  `(handler-case
       (progn ,@body nil)
     (%cut () t)))

;;; Builtin goal dispatch

(defgeneric %goal-solver (predicate)
  (:documentation "Return the immutable builtin solver associated with PREDICATE."))

(defmethod %goal-solver (predicate)
  (declare (ignore predicate))
  nil)

(defun %check-goal-arity (goal minimum maximum)
  (let ((arity (length (rest goal))))
    (unless (and (<= minimum arity)
                 (or (null maximum) (<= arity maximum)))
      (%invalid-goal goal
                     "builtin ~S expects ~:[at least ~D~;~D~] argument~:P, got ~D"
                     (first goal) maximum (or maximum minimum) arity))))

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
supported); its arity is enforced against each goal before BODY runs.
BODY must call EMIT with one extended environment per solution."
  (multiple-value-bind (minimum maximum)
      (%argument-list-arity argument-list)
    (let ((goal (gensym "GOAL"))
          (names (if (listp name) name (list name))))
      `(progn
         ,@(mapcar
            (lambda (builtin-name)
              `(defmethod %goal-solver ((predicate (eql ',builtin-name)))
                 (declare (ignore predicate))
                 (lambda (,goal ,rulebase ,environment ,depth ,emit)
                   (declare (ignorable ,rulebase ,environment ,depth ,emit))
                   (%check-goal-arity ,goal ,minimum ,maximum)
                   (destructuring-bind ,argument-list (rest ,goal)
                     ,@body))))
            names)
         ',(first names)))))

;;; Foreign predicate hook

(defgeneric predicate-true-p (predicate args bindings)
  (:documentation
   "Extension hook: return true when the foreign PREDICATE succeeds.

Specialize with (EQL 'NAME) to make NAME provable from Lisp; the default
method fails so ordinary fact/rule search proceeds.")
  (:method ((predicate symbol) args bindings)
    (declare (ignore args bindings))
    nil))
