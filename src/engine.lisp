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

(in-package #:fx.prolog)

(defparameter *max-prolog-depth* 64
  "Default bound on rule-resolution depth during proof search.")

(define-condition invalid-goal-error (error)
  ((goal :initarg :goal :reader invalid-goal-error-goal)
   (reason :initarg :reason :reader invalid-goal-error-reason))
  (:report (lambda (condition stream)
             (format stream "Invalid Prolog goal ~S: ~A."
                     (invalid-goal-error-goal condition)
                     (invalid-goal-error-reason condition))))
  (:documentation "Signalled when a goal is structurally unusable."))

(defun %invalid-goal (goal reason &rest arguments)
  (error 'invalid-goal-error
         :goal goal
         :reason (apply #'format nil reason arguments)))

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

(defmacro %with-depth-guard (depth &body body)
  "Run BODY unless DEPTH is exhausted."
  `(unless (minusp ,depth)
     ,@body))

;;; Builtin goal registry

(defvar *builtin-solvers* (make-hash-table :test #'eq)
  "Map from goal head symbol to its solver function.

A solver receives (GOAL RULEBASE ENVIRONMENT DEPTH EMIT) and calls EMIT
with one extended environment per solution.")

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

(defun %register-builtin-solver (names solver)
  "Register SOLVER for each symbol in NAMES and return the primary name."
  (dolist (builtin-name names (first names))
    (setf (gethash builtin-name *builtin-solvers*) solver)))

(defun %register-builtins (names minimum maximum implementation)
  "Register IMPLEMENTATION as a builtin shared by NAMES.

IMPLEMENTATION receives (ARGUMENTS RULEBASE ENVIRONMENT DEPTH EMIT), where
ARGUMENTS is the raw goal tail after arity validation."
  (%register-builtin-solver
   names
   (lambda (goal rulebase environment depth emit)
     (%check-goal-arity goal minimum maximum)
     (funcall implementation (rest goal) rulebase environment depth emit))))

(defmacro define-builtin ((name &rest argument-list) (rulebase environment depth emit)
                          &body body)
  "Register a builtin solver for goals of shape (NAME . ARGUMENT-LIST).

NAME may also be a list of head symbols sharing one solver.  ARGUMENT-LIST
is an ordinary lambda list (only required parameters and &REST are
supported); its arity is enforced against each goal before BODY runs.
BODY must call EMIT with one extended environment per solution."
  (multiple-value-bind (minimum maximum)
      (%argument-list-arity argument-list)
    (let ((arguments (gensym "ARGUMENTS"))
          (names (if (listp name) name (list name))))
      `(%register-builtins
        ',names
        ,minimum
        ,maximum
        (lambda (,arguments ,rulebase ,environment ,depth ,emit)
          (declare (ignorable ,rulebase ,environment ,depth ,emit))
          (destructuring-bind ,argument-list ,arguments
            ,@body))))))

;;; Foreign predicate hook

(defgeneric predicate-true-p (predicate args bindings)
  (:documentation
   "Extension hook: return true when the foreign PREDICATE succeeds.

Specialize with (EQL 'NAME) to make NAME provable from Lisp; the default
method fails so ordinary fact/rule search proceeds.")
  (:method ((predicate symbol) args bindings)
    (declare (ignore args bindings))
    nil))
