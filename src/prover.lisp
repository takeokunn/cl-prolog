;;;; Goal normalization and CPS proof search.
;;;;
;;;; The engine keeps clause data and proof search separate: queries are
;;;; normalized here, then proven against the builtin registry, foreign
;;;; predicate hook, facts, and rules.

(in-package #:cl-prolog)

(declaim (ftype function %prove-goal/k %prove-clauses/k %prove-rule/k))

(defun %prove-goals/k (goals state succeed)
  "Prove conjunction GOALS, calling SUCCEED with each solution state.

Each goal's solutions continue into the rest of the conjunction rebased
onto this conjunction's own cut barrier, so a callee's barrier never leaks
into the caller's remaining goals."
  (if (endp goals)
      (funcall succeed state)
      (let ((cut-tag (proof-state-cut-tag state)))
        (%prove-goal/k (first goals) state
                       (lambda (next-state)
                         (%prove-goals/k (rest goals)
                                         (%state-with-cut-tag next-state cut-tag)
                                         succeed))))))

(defun %normalize-dispatched-goal (goal state)
  "Return the normalized goal and its optional explicit module."
  (let* ((qualified-p (%qualified-goal-p goal))
         (explicit-module (and qualified-p
                               (%resolve-qualified-module (second goal) state)))
         (normalized-goal
           (%ensure-goal-form (if qualified-p (third goal) goal))))
    (unless (%goal-form-p normalized-goal)
      (%invalid-goal goal "a goal must be a symbol or a list headed by a symbol"))
    (values normalized-goal explicit-module)))

(defun %prove-with-solver/k (solver goal state explicit-module succeed)
  "Invoke SOLVER for GOAL and pass each resulting state to SUCCEED."
  (when explicit-module
    (%find-prolog-module
     (rulebase-module-registry (proof-state-rulebase state))
     explicit-module "invoke qualified goal"))
  (let* ((solver-state (if explicit-module
                           (%state-in-module state explicit-module)
                           state))
         (*current-prolog-module* (proof-state-module solver-state))
         (*caller-cut-tag* (proof-state-cut-tag solver-state)))
    (funcall solver
             goal
             (proof-state-rulebase solver-state)
             (proof-state-bindings solver-state)
             (proof-state-remaining-depth solver-state)
             (lambda (bindings)
               (funcall succeed (%state-with-bindings solver-state bindings))))))

(defun %prove-user-goal/k (goal state explicit-module succeed)
  "Resolve and prove a user-defined GOAL."
  (multiple-value-bind (resolved-goal defining-module)
      (%resolve-user-goal goal state explicit-module)
    (if defining-module
        (%prove-clauses/k resolved-goal
                          (%state-in-module state defining-module)
                          succeed)
        (%raise-existence-error
         "PROCEDURE" (%goal-predicate-indicator goal)
         (proof-state-bindings state) (%iso-atom "CALL")
         "the invoked predicate is not defined"))))

(defun %prove-goal-dispatch/k (goal state succeed)
  "Prove GOAL from STATE after any active depth-limit accounting."
  (let ((*current-table-session* (proof-state-table-session state)))
    (multiple-value-bind (normalized-goal explicit-module)
        (%normalize-dispatched-goal goal state)
      (when (equal normalized-goal (quote (!)))
        (funcall succeed state)
        (cl:throw (proof-state-cut-tag state) t))
      (let* ((predicate (first normalized-goal))
             (arity (length (rest normalized-goal)))
             (solver (or (%goal-solver predicate arity)
                         (%foreign-goal-solver predicate arity))))
        (if solver
            (%prove-with-solver/k solver normalized-goal state
                                  explicit-module succeed)
            (%prove-user-goal/k normalized-goal state
                                explicit-module succeed))))))

(defun %prove-goal/k (goal state succeed)
  "Prove GOAL, counting every dispatched call for local depth limits."
  (if (null *call-depth-limit-token*)
      (%prove-goal-dispatch/k goal state succeed)
      (progn
        (when (zerop *call-depth-limit-remaining*)
          (cl:throw *call-depth-limit-token* *call-depth-limit-token*))
        (let ((*call-depth-limit-remaining*
                (1- *call-depth-limit-remaining*))
              (*call-depth-limit-used*
                (1+ *call-depth-limit-used*)))
          (%prove-goal-dispatch/k goal state succeed)))))

(defun %prove-with-cut-tag/k (query rulebase bindings remaining-depth cut-tag
                              succeed &optional (module *current-prolog-module*))
  "Prove QUERY under an existing cut barrier CUT-TAG."
  (%prove-goals/k (%normalize-query query)
                  (%make-proof-state rulebase bindings remaining-depth module
                                     (or *current-table-session*
                                         (%make-rulebase-table-session rulebase))
                                     cut-tag)
                  (lambda (state)
                    (funcall succeed (proof-state-bindings state)))))

(defun %prove-bindings/k (query rulebase bindings remaining-depth succeed
                          &optional (module *current-prolog-module*))
  "Prove QUERY and call SUCCEED with each resulting binding environment.

QUERY runs behind its own cut barrier: a cut inside it never prunes the
caller's alternatives, matching ISO CALL/1 opacity."
  (let ((cut-tag (%make-cut-tag)))
    (cl:catch cut-tag
      (%prove-with-cut-tag/k query rulebase bindings remaining-depth cut-tag
                             succeed module))))

(defun %prove-transparent/k (query rulebase bindings remaining-depth succeed)
  "Prove QUERY sharing the caller's cut barrier.

Cut-transparent control constructs must call this at solver entry, before
any nested proof rebinds *CALLER-CUT-TAG*."
  (%prove-with-cut-tag/k query rulebase bindings remaining-depth
                         *caller-cut-tag* succeed))

(defun %replay-table-answers/k (goal state entry succeed)
  "Unify each stored answer for ENTRY with GOAL and invoke SUCCEED."
  (dolist (answer (%table-entry-answers entry))
    (multiple-value-bind (extended ok)
        (unify goal (%instantiate-variant answer) (proof-state-bindings state))
      (when ok
        (funcall succeed (%state-with-bindings state extended))))))

(defun %prove-clause/k (goal clause state succeed)
  "Try one stored CLAUSE for GOAL."
  (if (null (clause-body clause))
      (%continue-matching-fact goal clause state succeed)
      (when (%matching-rule-p goal clause)
        (%prove-rule/k goal clause state succeed))))

(defun %prove-raw-clauses/k (goal state succeed)
  "Prove GOAL within one predicate invocation and consume its cut.

The fresh CATCH tag is this invocation's cut barrier: a cut in any clause
body throws here, abandoning the remaining clause alternatives."
  (let* ((cut-tag (%make-cut-tag))
         (state (%state-with-cut-tag state cut-tag)))
    (cl:catch cut-tag
      (dolist (entry (%proof-predicate-entries goal state))
        (%prove-clause/k goal (%stored-clause-clause entry) state succeed)))))

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
         (rulebase (proof-state-rulebase state))
         (module (proof-state-module state))
         (cache (%table-session-left-recursion
                 (proof-state-table-session state)))
         (cache-key (list (rulebase-revision rulebase) module
                          (car target) (cdr target))))
    (multiple-value-bind (cached present-p) (gethash cache-key cache)
      (if present-p
          cached
          (let ((entries (%proof-module-entries state))
                (visited (make-hash-table :test #'equal)))
            (labels ((reaches-target-p (key)
                       (some (lambda (entry)
                               (let* ((clause (%stored-clause-clause entry))
                                      (head-key (%predicate-key
                                                 (clause-head clause)))
                                      (successor
                                        (%first-user-predicate-key clause)))
                                 (and (equal key head-key)
                                      successor
                                      (or (equal successor target)
                                          (unless (gethash successor visited)
                                            (setf (gethash successor visited) t)
                                            (reaches-target-p successor))))))
                             entries)))
              (setf (gethash target visited) t
                    (gethash cache-key cache)
                    (not (null (reaches-target-p target))))))))))

(defun %table-goal-key (goal state)
  "Return the variant-table key for GOAL in STATE."
  (list (rulebase-revision (proof-state-rulebase state))
        (proof-state-module state)
        (%canonicalize-variant
         (logic-substitute goal (proof-state-bindings state)))))

(defun %record-table-answer! (goal answer-state entry)
  "Record a new tabled answer and report whether ENTRY changed."
  (let ((answer (%canonicalize-variant
                 (logic-substitute goal
                                   (proof-state-bindings answer-state)))))
    (unless (member answer (%table-entry-answers entry) :test #'equal)
      (setf (%table-entry-answers entry)
            (append (%table-entry-answers entry) (list answer)))
      t)))

(defun %prove-clauses/k (goal state succeed)
  "Prove GOAL, tabling declared predicates and detected left recursion."
  (if (or *depth-limited-search-p*
          (and *constraints-active-p-hook*
               (funcall *constraints-active-p-hook*))
          (not (or (%rulebase-tabled-p
                    (proof-state-rulebase state) (first goal)
                    (length (rest goal)) (proof-state-module state))
                   (%left-recursive-p goal state))))
      (%prove-raw-clauses/k goal state succeed)
      (let* ((session (proof-state-table-session state))
             (key (%table-goal-key goal state))
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
                             (when (%record-table-answer!
                                    goal answer-state entry)
                               (setf changed-p t)
                               (funcall succeed answer-state))))
                       while changed-p)
                     (setf completed-p t))
                (unless completed-p
                  (remhash key entries))))))))

(defun %prove-rule-body/k (goal fresh-rule state bindings succeed)
  "Continue a matched rule body with propagated BINDINGS."
  (%prove-goals/k
   (clause-body fresh-rule)
   (%state-descending-into-rule state bindings goal)
   succeed))

(defun %prove-rule/k (goal clause state succeed)
  "Resolve GOAL against one CLAUSE; a cut in the body prunes the clause list."
  (let ((fresh-rule (%freshen-clause clause)))
    (multiple-value-bind (extended ok)
        (unify goal (clause-head fresh-rule) (proof-state-bindings state))
      (when ok
        (if *constraint-post-unify-hook*
            (funcall *constraint-post-unify-hook*
                     extended
                     (lambda (propagated)
                       (%prove-rule-body/k
                        goal fresh-rule state propagated succeed)))
            (%prove-rule-body/k goal fresh-rule state extended succeed))))))

(defun %provable-p (query rulebase environment depth
                    &optional (module +default-prolog-module+))
  "Return true when QUERY has at least one proof."
  (%with-logic-variable-order
    (block provable
      (let ((cut-tag (%make-cut-tag)))
        (cl:catch cut-tag
          (%prove-goals/k (%normalize-query query)
                          (%make-proof-state rulebase environment depth
                                             module
                                             (%make-rulebase-table-session rulebase)
                                             cut-tag)
                          (lambda (state)
                            (declare (cl:ignore state))
                            (return-from provable t)))))
      nil)))
