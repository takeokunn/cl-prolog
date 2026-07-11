;;;; Goal normalization and CPS proof search.
;;;;
;;;; The engine keeps clause data and proof search separate: queries are
;;;; normalized here, then proven against the builtin registry, foreign
;;;; predicate hook, facts, and rules.

(in-package #:cl-prolog)

(declaim (ftype function %prove-goal/k %prove-clauses/k %prove-rule/k))

(defvar *current-prolog-module* +default-prolog-module+)
(defvar *current-table-session* nil
  "Table session inherited by proof searches nested through builtins.")

(defstruct (proof-state
            (:constructor %make-proof-state
                (rulebase bindings remaining-depth module table-session)))
  "Immutable data carried through the proof-search continuation."
  (rulebase (make-rulebase) :type rulebase :read-only t)
  (bindings '() :type list :read-only t)
  (module +default-prolog-module+ :type symbol :read-only t)
  (table-session nil :type (or null %table-session) :read-only t)
  (remaining-depth *max-prolog-depth*
                   :type (or null (integer 0 *))
                   :read-only t))

(defun %state-with-bindings (state bindings)
  "Return STATE advanced with BINDINGS while preserving its search budget."
  (%make-proof-state (proof-state-rulebase state)
                     bindings
                     (proof-state-remaining-depth state)
                     (proof-state-module state)
                     (proof-state-table-session state)))

(defun %state-in-module (state module)
  (%make-proof-state (proof-state-rulebase state)
                     (proof-state-bindings state)
                     (proof-state-remaining-depth state)
                     module
                     (proof-state-table-session state)))

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
                       (and remaining (1- remaining))
                       (proof-state-module state)
                       (proof-state-table-session state))))

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

(defun %goal-predicate-indicator (goal)
  "Return the ISO predicate indicator for normalized GOAL."
  (list (first goal) '/ (length (rest goal))))

(defun %clause-defines-goal-p (clause goal)
  "True when CLAUSE defines the same predicate and arity as GOAL."
  (let ((head (clause-head clause)))
    (and (consp head)
         (eq (first head) (first goal))
         (= (length (rest head)) (length (rest goal))))))

(defun %rulebase-defines-goal-p (rulebase module goal)
  "True when RULEBASE contains or declares GOAL's predicate."
  (let ((predicate (first goal))
        (arity (length (rest goal))))
    (or (%rulebase-predicate-property rulebase predicate arity module)
        (some (lambda (entry)
                (%clause-defines-goal-p (%stored-clause-clause entry) goal))
              (%rulebase-module-entries rulebase module)))))

(defun %qualified-goal-p (goal)
  (and (consp goal) (= (length goal) 3)
       (eq (first goal) (%prolog-symbol ":"))))

(defun %resolve-user-goal (goal state &optional explicit-module)
  (let* ((rulebase (proof-state-rulebase state))
         (registry (rulebase-module-registry rulebase))
         (caller (or explicit-module (proof-state-module state)))
         (predicate (first goal))
         (arity (length (rest goal)))
         (local-p
           (lambda (module name count)
             (%rulebase-defines-goal-p
              rulebase module (list* name (make-list count))))))
    (values goal
            (if explicit-module
                (module-registry-resolve-qualified
                 registry caller predicate arity local-p)
                (module-registry-resolve
                 registry caller predicate arity local-p)))))

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
  (let* ((*current-table-session* (proof-state-table-session state))
         (qualified-p (%qualified-goal-p goal))
         (explicit-module (and qualified-p (second goal)))
         (normalized-goal
           (%ensure-goal-form (if qualified-p (third goal) goal))))
    (cond
      ((not (%goal-form-p normalized-goal))
       (%invalid-goal goal "a goal must be a symbol or a list headed by a symbol"))
      (t
       (let* ((predicate (first normalized-goal))
              (arity (length (rest normalized-goal)))
              (builtin-solver (%goal-solver predicate arity))
              (foreign-solver (%foreign-goal-solver predicate arity))
              (solver (or builtin-solver foreign-solver)))
         (cond
           (solver
             (let ((*current-prolog-module* (proof-state-module state)))
               (funcall solver
                        normalized-goal
                        (proof-state-rulebase state)
                        (proof-state-bindings state)
                        (proof-state-remaining-depth state)
                        (lambda (bindings)
                          (funcall succeed
                                   (%state-with-bindings state bindings))))))
           (t
            (multiple-value-bind (resolved-goal defining-module)
                (%resolve-user-goal normalized-goal state explicit-module)
              (if defining-module
                  (%prove-clauses/k resolved-goal
                                    (%state-in-module state defining-module)
                                    succeed)
                  (%raise-existence-error
                   "PROCEDURE" (%goal-predicate-indicator normalized-goal)
                   (proof-state-bindings state) (%iso-atom "CALL")
                   "the invoked predicate is not defined"))))))))))

(defun %prove-bindings/k (query rulebase bindings remaining-depth succeed
                          &optional (module *current-prolog-module*))
  "Prove QUERY and call SUCCEED with each resulting binding environment."
  (%prove-goals/k (%normalize-query query)
                  (%make-proof-state rulebase bindings remaining-depth module
                                     (or *current-table-session*
                                         (%make-rulebase-table-session rulebase)))
                  (lambda (state)
                    (funcall succeed (proof-state-bindings state)))))

(defun %replay-table-answers/k (goal state entry succeed)
  "Unify each stored answer for ENTRY with GOAL and invoke SUCCEED."
  (dolist (answer (%table-entry-answers entry))
    (multiple-value-bind (extended ok)
        (unify goal (%instantiate-variant answer) (proof-state-bindings state))
      (when ok
        (funcall succeed (%state-with-bindings state extended))))))

(defun %prove-raw-clauses/k (goal state succeed)
  "Prove GOAL within one predicate invocation and consume its cut."
  (%with-cut-barrier
    (dolist (entry (%rulebase-module-entries (proof-state-rulebase state)
                                             (proof-state-module state)))
      (let ((clause (%stored-clause-clause entry)))
        (if (null (clause-body clause))
            (%continue-matching-fact goal clause state succeed)
            (when (%matching-rule-p goal clause)
              (%prove-rule/k goal clause state succeed)))))))

(defun %predicate-key (goal)
  (when (%goal-form-p goal)
    (cons (first goal) (length (rest goal)))))

(defun %first-user-predicate-key (clause)
  "Return CLAUSE's first user-defined predicate indicator, if any."
  (let* ((goal (%ensure-goal-form (first (clause-body clause))))
         (key (%predicate-key goal)))
    (when (and key
               (not (%goal-solver (car key) (cdr key)))
               (not (%foreign-goal-solver (car key) (cdr key))))
      key)))

(defun %left-recursive-p (goal state)
  "Return true when GOAL reaches itself through first user-goal calls."
  (let* ((target (%predicate-key goal))
         (entries (%rulebase-module-entries (proof-state-rulebase state)
                                            (proof-state-module state)))
         (visited (make-hash-table :test #'equal)))
    (labels ((successors (key)
               (loop for entry in entries
                     for clause = (%stored-clause-clause entry)
                     for head-key = (%predicate-key (clause-head clause))
                     for successor = (%first-user-predicate-key clause)
                     when (and (equal key head-key) successor)
                       collect successor))
             (reaches-target-p (key)
               (some (lambda (successor)
                       (or (equal successor target)
                           (unless (gethash successor visited)
                             (setf (gethash successor visited) t)
                             (reaches-target-p successor))))
                     (successors key))))
      (setf (gethash target visited) t)
      (reaches-target-p target))))

(defun %prove-clauses/k (goal state succeed)
  "Prove GOAL, tabling only predicates that require a left-recursion fixed point."
  (if (not (%left-recursive-p goal state))
      (%prove-raw-clauses/k goal state succeed)
      (let* ((session (proof-state-table-session state))
         (resolved-goal (logic-substitute goal (proof-state-bindings state)))
         (key (list (proof-state-module state)
                    (%canonicalize-variant resolved-goal)))
         (entries (%table-session-entries session))
         (entry (gethash key entries)))
    (if entry
        (%replay-table-answers/k goal state entry succeed)
        (let ((entry (%make-table-entry))
              (completed-p nil))
          (setf (gethash key entries) entry)
          (unwind-protect
               (progn
                 (loop
                   with changed-p
                   do (setf changed-p nil)
                      (%prove-raw-clauses/k
                       goal state
                       (lambda (answer-state)
                         (let ((answer
                                 (%canonicalize-variant
                                  (logic-substitute
                                   goal (proof-state-bindings answer-state)))))
                           (unless (member answer (%table-entry-answers entry)
                                           :test #'equal)
                             (setf (%table-entry-answers entry)
                                   (append (%table-entry-answers entry)
                                           (list answer))
                                   changed-p t)
                             (funcall succeed answer-state)))))
                   while changed-p)
                 (setf completed-p t))
            (unless completed-p
              (remhash key entries))))))))

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
                          (%make-proof-state rulebase environment depth
                                             +default-prolog-module+
                                             (%make-rulebase-table-session rulebase))
                          (lambda (state)
                            (declare (cl:ignore state))
                            (return-from provable t)))
    nil))
