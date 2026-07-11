;;;; Builtin goal solvers.
;;;;
;;;; Every builtin is declared with DEFINE-BUILTIN and follows the engine's
;;;; CPS contract: emit each solution environment as it is found, never
;;;; collect intermediate result lists.

(in-package #:cl-prolog)

(defun %unify-emit (left right environment emit)
  "EMIT the extension of ENVIRONMENT that unifies LEFT with RIGHT, if any."
  (multiple-value-bind (extended ok)
      (unify left right environment)
    (when ok
      (funcall emit extended))))

(defun %proper-list-p (value)
  "True when VALUE is a fully-instantiated proper list."
  (cond
    ((null value) t)
    ((consp value) (%proper-list-p (cdr value)))
    (t nil)))

(defun %entry-head (clause)
  (clause-head clause))

(defun %entry-body-term (clause)
  (if (null (clause-body clause))
      'true
      (let ((body (clause-body clause)))
        (cond
          ((null body) 'true)
          ((null (rest body)) (first body))
          (t (cons 'and body))))))

(defun %freshen-dynamic-clause (clause)
  (%freshen-clause clause))

(defun %builtin-predicate-p (predicate)
  (and (symbolp predicate) (%goal-solver predicate)))

(defun %ensure-dynamic-predicate (predicate goal)
  (when (%builtin-predicate-p predicate)
    (%invalid-goal goal "builtin predicate ~S cannot be inspected or modified" predicate)))

(defun %clause-term-entry (term goal)
  "Convert a substituted dynamic clause TERM to a freshly renamed entry."
  (cond
    ((and (%proper-list-p term) (eq (first term) ':-))
     (unless (and (consp (rest term))
                  (%goal-form-p (second term)))
       (%invalid-goal goal "a rule must have shape (:- (PREDICATE . ARGS) GOAL...)"))
     (%ensure-dynamic-predicate (first (second term)) goal)
     (let ((body (cddr term)))
       (unless (every #'%goal-form-p (mapcar #'%ensure-goal-form body))
         (%invalid-goal goal "every rule body element must be a callable goal"))
       (%freshen-clause (make-clause (second term) body))))
    ((%goal-form-p term)
     (%ensure-dynamic-predicate (first term) goal)
     (let ((table (make-hash-table :test #'eq)))
       (make-clause (%freshen-term term table))))
    (t
     (%invalid-goal goal "a dynamic clause must be a fact or (:- HEAD BODY...)"))))

(defun %entry-predicate-arity (entry)
  (let ((head (%entry-head entry)))
    (values (first head) (length (rest head)))))
