;;;; Goal normalization and CPS proof search.
;;;;
;;;; The engine keeps clause data and proof search separate: queries are
;;;; normalized here, then proven against the builtin registry, foreign
;;;; predicate hook, facts, and rules.

(in-package #:cl-prolog)

(declaim (ftype function %prove-goal/k %prove-clauses/k %prove-rule/k))

(defstruct (proof-state
            (:constructor %make-proof-state (rulebase bindings remaining-depth)))
  "Immutable data carried through the proof-search continuation."
  (rulebase (make-rulebase) :type rulebase :read-only t)
  (bindings '() :type list :read-only t)
  (remaining-depth *max-prolog-depth*
                   :type (or null (integer 0 *))
                   :read-only t))

(defun %state-with-bindings (state bindings)
  "Return STATE advanced with BINDINGS while preserving its search budget."
  (%make-proof-state (proof-state-rulebase state)
                     bindings
                     (proof-state-remaining-depth state)))

(defun %state-descending-into-rule (state bindings goal)
  "Return the state for proving a matched rule body."
  (let ((remaining (proof-state-remaining-depth state)))
    (when (eql remaining 0)
      (%raise-resource-error "DEPTH_LIMIT"
                             (proof-state-bindings state)
                             (%iso-atom "CALL")
                             "explicit rule-resolution depth limit exceeded"
                             :condition-type 'prolog-depth-limit-exceeded
                             :goal goal))
    (%make-proof-state (proof-state-rulebase state)
                       bindings
                       (and remaining (1- remaining)))))

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

(defun %continue-foreign-proof (goal state succeed)
  "Call SUCCEED with STATE when the foreign predicate hook proves GOAL."
  (when (predicate-true-p (first goal) (rest goal)
                          (proof-state-bindings state))
    (funcall succeed state)))

(defun %continue-matching-fact (goal clause state succeed)
  "Unify GOAL against fact CLAUSE and continue with the extended state."
  (when (eq (first goal) (first (clause-head clause)))
    (multiple-value-bind (extended ok)
        (unify goal (clause-head (%freshen-clause clause))
               (proof-state-bindings state))
      (when ok
        (funcall succeed (%state-with-bindings state extended))))))

(defun %matching-rule-p (goal clause)
  "True when CLAUSE can be considered for GOAL."
  (and (consp (clause-head clause))
       (eq (first goal) (first (clause-head clause)))))

(defun %prove-goals/k (goals state succeed)
  "Prove conjunction GOALS, calling SUCCEED with each solution state.

Return true when a cut fired inside GOALS, so the caller can prune its own
alternatives as well."
  (if (endp goals)
      (progn (funcall succeed state) nil)
      (%with-cut-barrier
        (%prove-goal/k (first goals) state
                       (lambda (next-state)
                         (when (%prove-goals/k (rest goals) next-state succeed)
                           (%propagate-cut)))))))

(defun %prove-goal/k (goal state succeed)
  "Prove GOAL from STATE, dispatching each result to SUCCEED."
  (let ((normalized-goal (%ensure-goal-form goal)))
    (cond
      ((not (%goal-form-p normalized-goal))
       (%invalid-goal goal "a goal must be a symbol or a list headed by a symbol"))
      (t
       (let ((solver (%goal-solver (first normalized-goal))))
         (if solver
             (funcall solver
                      normalized-goal
                      (proof-state-rulebase state)
                      (proof-state-bindings state)
                      (proof-state-remaining-depth state)
                      (lambda (bindings)
                        (funcall succeed (%state-with-bindings state bindings))))
             (%prove-clauses/k normalized-goal state succeed)))))))

(defun %prove-bindings/k (query rulebase bindings remaining-depth succeed)
  "Prove QUERY and call SUCCEED with each resulting binding environment."
  (%prove-goals/k (%normalize-query query)
                  (%make-proof-state rulebase bindings remaining-depth)
                  (lambda (state)
                    (funcall succeed (proof-state-bindings state)))))

(defun %prove-clauses/k (goal state succeed)
  "Prove GOAL against the foreign hook and a logical-update-view snapshot."
  (%continue-foreign-proof goal state succeed)
  (dolist (clause (rulebase-visible-clauses (proof-state-rulebase state)))
    (if (null (clause-body clause))
        (%continue-matching-fact goal clause state succeed)
        (when (%matching-rule-p goal clause)
          (%prove-rule/k goal clause state succeed)))))

(defun %prove-rule/k (goal clause state succeed)
  "Resolve GOAL against one CLAUSE; a cut in the body prunes the clause list."
  (let ((fresh-rule (%freshen-clause clause)))
    (multiple-value-bind (extended ok)
        (unify goal (clause-head fresh-rule) (proof-state-bindings state))
      (when ok
        (when (%prove-goals/k
               (clause-body fresh-rule)
               (%state-descending-into-rule state extended goal)
               succeed)
          (%propagate-cut))))))

(defun %provable-p (query rulebase environment depth)
  "Return true when QUERY has at least one proof."
  (block provable
    (%prove-goals/k (%normalize-query query)
                          (%make-proof-state rulebase environment depth)
                          (lambda (state)
                            (declare (ignore state))
                            (return-from provable t)))
    nil))
