;;;; Goal normalization and CPS proof search.
;;;;
;;;; The engine keeps clause data and proof search separate: queries are
;;;; normalized here, then proven against the builtin registry, foreign
;;;; predicate hook, facts, and rules.

(in-package #:fx.prolog)

(declaim (ftype function %prove-goal %prove-with-clauses %prove-with-rule))

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

(defun %goal-form-p (goal)
  "True when GOAL is a proper callable goal form."
  (and (consp goal)
       (symbolp (first goal))))

(defun %ensure-goal-form (goal)
  "Normalize bare-symbol goals to list form."
  (if (symbolp goal)
      (list goal)
      goal))

(defun %emit-foreign-proof (goal environment emit)
  "Emit ENVIRONMENT when the foreign predicate hook proves GOAL."
  (when (predicate-true-p (first goal) (rest goal) environment)
    (funcall emit environment)))

(defun %emit-matching-fact (goal clause environment emit)
  "Unify GOAL against fact CLAUSE and emit each successful environment."
  (when (eq (first goal) (first (clause-head clause)))
    (multiple-value-bind (extended ok)
        (unify goal (clause-head (%freshen-clause clause)) environment)
      (when ok
        (funcall emit extended)))))

(defun %matching-rule-p (goal clause)
  "True when CLAUSE can be considered for GOAL."
  (and (consp (clause-head clause))
       (eq (first goal) (first (clause-head clause)))))

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
    (let ((normalized-goal (%ensure-goal-form goal)))
      (cond
        ((not (%goal-form-p normalized-goal))
         (%invalid-goal goal "a goal must be a symbol or a list headed by a symbol"))
        (t
         (let ((solver (%goal-solver (first normalized-goal))))
           (if solver
               (funcall solver normalized-goal rulebase environment depth emit)
               (%prove-with-clauses normalized-goal rulebase environment depth emit))))))))

(defun %prove-with-clauses (goal rulebase environment depth emit)
  "Prove GOAL against the foreign hook and a logical-update-view snapshot."
  (%emit-foreign-proof goal environment emit)
  (dolist (clause (copy-list (rulebase-clauses rulebase)))
    (if (null (clause-body clause))
        (%emit-matching-fact goal clause environment emit)
        (when (and (plusp depth) (%matching-rule-p goal clause))
          (%prove-with-rule goal clause rulebase environment depth emit)))))

(defun %prove-with-rule (goal clause rulebase environment depth emit)
  "Resolve GOAL against one CLAUSE; a cut in the body prunes the clause list."
  (let ((fresh-rule (%freshen-clause clause)))
    (multiple-value-bind (extended ok)
        (unify goal (clause-head fresh-rule) environment)
      (when ok
        (when (%prove-goal-sequence (clause-body fresh-rule) rulebase
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
