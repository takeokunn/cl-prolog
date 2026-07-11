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

(defun %goal-solver (goal)
  "Return the builtin solver for GOAL, or NIL when clause search should run."
  (gethash (first goal) *builtin-solvers*))

(defun %emit-foreign-proof (goal environment emit)
  "Emit ENVIRONMENT when the foreign predicate hook proves GOAL."
  (when (predicate-true-p (first goal) (rest goal) environment)
    (funcall emit environment)))

(defun %emit-matching-fact (goal fact environment emit)
  "Unify GOAL against FACT and emit each successful environment."
  (when (eq (first goal) (fact-predicate fact))
    (multiple-value-bind (extended ok)
        (unify (rest goal) (%freshen-fact-args fact) environment)
      (when ok
        (funcall emit extended)))))

(defun %matching-rule-p (goal rule)
  "True when RULE can be considered for GOAL."
  (and (consp (rule-head rule))
       (eq (first goal) (first (rule-head rule)))))

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
         (let ((solver (%goal-solver normalized-goal)))
           (if solver
               (funcall solver normalized-goal rulebase environment depth emit)
               (%prove-with-clauses normalized-goal rulebase environment depth emit))))))))

(defun %prove-with-clauses (goal rulebase environment depth emit)
  "Prove GOAL against the foreign hook and a logical-update-view snapshot."
  (%emit-foreign-proof goal environment emit)
  (dolist (entry (copy-list (rulebase-clauses rulebase)))
    (ecase (clause-entry-kind entry)
      (:fact
       (%emit-matching-fact goal (clause-entry-clause entry) environment emit))
      (:rule
       (let ((rule (clause-entry-clause entry)))
         (when (and (plusp depth) (%matching-rule-p goal rule))
           (%prove-with-rule goal rule rulebase environment depth emit)))))))

(defun %prove-with-rule (goal rule rulebase environment depth emit)
  "Resolve GOAL against one RULE; a cut in the body prunes the clause list."
  (let ((fresh-rule (%freshen-rule rule)))
    (multiple-value-bind (extended ok)
        (unify goal (rule-head fresh-rule) environment)
      (when ok
        (when (%prove-goal-sequence (rule-body fresh-rule) rulebase
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
