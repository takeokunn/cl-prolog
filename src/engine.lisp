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

(defmacro define-builtin ((name &rest argument-list) (rulebase environment depth emit)
                          &body body)
  "Register a builtin solver for goals of shape (NAME . ARGUMENT-LIST).

NAME may also be a list of head symbols sharing one solver.  ARGUMENT-LIST
is an ordinary lambda list (only required parameters and &REST are
supported); its arity is enforced against each goal before BODY runs.
BODY must call EMIT with one extended environment per solution."
  (multiple-value-bind (minimum maximum)
      (%argument-list-arity argument-list)
    (let ((goal (gensym "GOAL"))
          (solver (gensym "SOLVER"))
          (names (if (listp name) name (list name))))
      `(let ((,solver
               (lambda (,goal ,rulebase ,environment ,depth ,emit)
                 (declare (ignorable ,rulebase ,environment ,depth ,emit))
                 (%check-goal-arity ,goal ,minimum ,maximum)
                 (destructuring-bind ,argument-list (rest ,goal)
                   ,@body))))
         (dolist (builtin-name ',names ',(first names))
           (setf (gethash builtin-name *builtin-solvers*) ,solver))))))

;;; Foreign predicate hook

(defgeneric predicate-true-p (predicate args bindings)
  (:documentation
   "Extension hook: return true when the foreign PREDICATE succeeds.

Specialize with (EQL 'NAME) to make NAME provable from Lisp; the default
method fails so ordinary fact/rule search proceeds.")
  (:method ((predicate symbol) args bindings)
    (declare (ignore args bindings))
    nil))

;;; Query normalization

(defun %conjunction-p (query)
  "True when QUERY is already a list of goals rather than a single goal."
  (and (consp query)
       (or (consp (first query))
           (eq (first query) '!))))

(defun %normalize-query (query)
  "Coerce QUERY into a list of goals."
  (cond
    ((null query) '())
    ((%conjunction-p query) query)
    (t (list query))))

;;; Provers

(declaim (ftype function %prove-goal %prove-with-clauses %prove-with-rule))

(defun %prove-goal-sequence (goals rulebase environment depth emit)
  "Prove the conjunction GOALS, calling EMIT with each solution environment.

Return true when a cut fired inside GOALS, so the caller can prune its own
alternatives as well."
  (%with-depth-guard depth
    (if (endp goals)
        (progn (funcall emit environment) nil)
        (%with-cut-barrier
          (%prove-goal (first goals) rulebase environment depth
                       (lambda (extended)
                         (when (%prove-goal-sequence (rest goals) rulebase
                                                     extended depth emit)
                           (%propagate-cut))))))))

(defun %prove-goal (goal rulebase environment depth emit)
  "Prove a single GOAL, dispatching to builtins or clause search."
  (%with-depth-guard depth
    (cond
      ((symbolp goal)
       (%prove-goal (list goal) rulebase environment depth emit))
      ((and (consp goal) (symbolp (first goal)))
       (let ((solver (gethash (first goal) *builtin-solvers*)))
         (if solver
             (funcall solver goal rulebase environment depth emit)
             (%prove-with-clauses goal rulebase environment depth emit))))
      (t
       (%invalid-goal goal "a goal must be a symbol or a list headed by a symbol")))))

(defun %prove-with-clauses (goal rulebase environment depth emit)
  "Prove GOAL against the foreign hook, then facts, then rules."
  (let ((predicate (first goal)))
    (when (predicate-true-p predicate (rest goal) environment)
      (funcall emit environment))
    (dolist (fact (rulebase-facts rulebase))
      (when (eq predicate (fact-predicate fact))
        (multiple-value-bind (extended ok)
            (unify (rest goal) (%freshen-fact-args fact) environment)
          (when ok
            (funcall emit extended)))))
    (when (plusp depth)
      (dolist (rule (rulebase-rules rulebase))
        (when (and (consp (rule-head rule))
                   (eq predicate (first (rule-head rule))))
          (%prove-with-rule goal rule rulebase environment depth emit))))))

(defun %prove-with-rule (goal rule rulebase environment depth emit)
  "Resolve GOAL against one RULE; a cut in the body prunes the clause list."
  (let ((rule (%freshen-rule rule)))
    (multiple-value-bind (extended ok)
        (unify goal (rule-head rule) environment)
      (when ok
        (when (%prove-goal-sequence (rule-body rule) rulebase
                                    extended (1- depth) emit)
          (%propagate-cut))))))

(defun %provable-p (query rulebase environment depth)
  "Return true when QUERY has at least one proof."
  (block provable
    (%prove-goal-sequence (%normalize-query query) rulebase environment depth
                          (lambda (extended)
                            (declare (ignore extended))
                            (return-from provable t)))
    nil))
