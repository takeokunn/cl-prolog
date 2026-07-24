;;;; Goal normalization and CPS proof search.
;;;;
;;;; The engine keeps clause data and proof search separate: queries are
;;;; normalized here, then proven against the builtin registry, foreign
;;;; predicate hook, facts, and rules.

(in-package #:cl-prolog)

(declaim (ftype function %proper-list-p %prove-goal/k %prove-clauses/k %prove-rule/k))

(defvar *current-prolog-module* +default-prolog-module+)
(defvar *current-table-session* nil
  "Table session inherited by proof searches nested through builtins.")
(defvar *call-depth-limit-token* nil)
(defvar *call-depth-limit-remaining* nil)
(defvar *call-depth-limit-used* 0)
(defvar *depth-limited-search-p* nil)
(defvar *constraint-post-unify-hook* nil
  "Function called after builtin =/2 extends an environment.")
(defvar *constraints-active-p-hook* nil
  "Function reporting whether a dynamically scoped constraint store is active.")

(defvar *caller-cut-tag* nil
  "Cut barrier of the goal invocation currently dispatching a builtin solver.

Cut-transparent control constructs (AND, OR, the THEN/ELSE branches of
IF-THEN-ELSE) read this at solver entry so a cut inside them prunes the
caller's clause alternatives, as ISO requires.")

(defun %make-cut-tag ()
  "Return a fresh CATCH tag identifying one cut barrier."
  (list '%cut-barrier))

(defstruct (proof-state
            (:constructor %make-proof-state
                (rulebase bindings remaining-depth module table-session
                 cut-tag)))
  "Immutable data carried through the proof-search continuation."
  (rulebase (make-rulebase) :type rulebase :read-only t)
  (bindings '() :type list :read-only t)
  (module +default-prolog-module+ :type symbol :read-only t)
  (table-session nil :type (or null %table-session) :read-only t)
  (cut-tag nil :type list :read-only t)
  (remaining-depth *max-prolog-depth*
                   :type (or null (integer 0 *))
                   :read-only t))

(defun %state-with (state &key (bindings nil bindings-p)
                                (module nil module-p)
                                (table-session nil table-session-p)
                                (cut-tag nil cut-tag-p)
                                (remaining-depth nil remaining-depth-p))
  "Return STATE with any supplied slot replaced; slots not supplied are
preserved from STATE.  Supplying only CUT-TAG, when it already matches
STATE's current cut tag, returns STATE unchanged, avoiding allocation
churn on the conjunction traversal's common no-op case."
  (if (and cut-tag-p (eq (proof-state-cut-tag state) cut-tag)
           (not bindings-p) (not module-p) (not table-session-p)
           (not remaining-depth-p))
      state
      (%make-proof-state (proof-state-rulebase state)
                         (if bindings-p bindings (proof-state-bindings state))
                         (if remaining-depth-p
                             remaining-depth
                             (proof-state-remaining-depth state))
                         (if module-p module (proof-state-module state))
                         (if table-session-p
                             table-session
                             (proof-state-table-session state))
                         (if cut-tag-p cut-tag (proof-state-cut-tag state)))))

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
    (%state-with state
                :bindings bindings
                :remaining-depth (and remaining (1- remaining)))))

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
       (%proper-list-p goal)
       (symbolp (first goal))))

(defun %ensure-goal-form (goal)
  "Normalize bare-symbol goals to list form."
  (if (symbolp goal)
      (list goal)
      goal))

(defun %goal-predicate-indicator (goal)
  "Return the ISO predicate indicator for normalized GOAL."
  (list '/ (first goal) (length (rest goal))))

(defun %proof-module-entries (state &optional (module (proof-state-module state)))
  "Return one revision-stable module snapshot shared by the current query."
  (let* ((rulebase (proof-state-rulebase state))
         (session (proof-state-table-session state))
         (key (list (rulebase-revision rulebase) module)))
    (multiple-value-bind (entries present-p)
        (gethash key (%table-session-module-entries session))
      (if present-p
          entries
          (setf (gethash key (%table-session-module-entries session))
                (%rulebase-module-entries rulebase module))))))

(defun %proof-predicate-entries (goal state
                                 &optional (module (proof-state-module state)))
  "Return one revision-stable indexed snapshot for GOAL's predicate."
  (let* ((rulebase (proof-state-rulebase state))
         (session (proof-state-table-session state))
         (revision (rulebase-revision rulebase))
         (predicate (first goal))
         (arity (length (rest goal)))
         (key (list revision module predicate arity)))
    (multiple-value-bind (entries present-p)
        (gethash key (%table-session-predicate-entries session))
      (if present-p
          entries
          (setf (gethash key (%table-session-predicate-entries session))
                (%rulebase-predicate-entries-at-revision
                 rulebase module predicate arity revision))))))

(defun %rulebase-defines-goal-p (state module goal)
  "True when RULEBASE contains or declares GOAL's predicate."
  (let ((rulebase (proof-state-rulebase state))
        (predicate (first goal))
        (arity (length (rest goal))))
    (or (%rulebase-predicate-property rulebase predicate arity module)
        (not (null (%proof-predicate-entries goal state module))))))

(defun %qualified-goal-p (goal)
  (and (%goal-form-p goal)
       (= (length goal) 3)
       (eq (first goal) (%prolog-symbol ":"))))

(defun %resolve-qualified-module (module state)
  "Resolve MODULE through the current bindings and validate it as a module atom."
  (let* ((environment (proof-state-bindings state))
         (resolved (logic-substitute module environment))
         (context (%iso-atom "CALL")))
    (when (logic-var-p resolved)
      (%raise-instantiation-error environment context
                                  "module qualifier must be instantiated"))
    (unless (symbolp resolved)
      (%raise-type-error "ATOM" resolved environment context
                         "module qualifier must be an atom"))
    (unless (gethash resolved
                     (module-registry-modules
                      (rulebase-module-registry (proof-state-rulebase state))))
      (%raise-existence-error "MODULE" resolved environment context
                              "unknown module"))
    resolved))

(defun %resolve-user-goal (goal state &optional explicit-module)
  (let* ((rulebase (proof-state-rulebase state))
         (registry (rulebase-module-registry rulebase))
         (caller (or explicit-module (proof-state-module state)))
         (predicate (first goal))
         (arity (length (rest goal)))
         (local-p
           (lambda (module name count)
             (%rulebase-defines-goal-p
              state module (list* name (make-list count))))))
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
        (flet ((continue-with-propagated-bindings (propagated)
                 (funcall succeed (%state-with state :bindings propagated))))
          (if *constraint-post-unify-hook*
              (funcall *constraint-post-unify-hook*
                       extended
                       #'continue-with-propagated-bindings)
              (continue-with-propagated-bindings extended)))))))

(defun %matching-rule-p (goal clause)
  "True when CLAUSE can be considered for GOAL."
  (and (consp (clause-head clause))
       (eq (first goal) (first (clause-head clause)))))

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
                                         (%state-with next-state :cut-tag cut-tag)
                                         succeed))))))

(defun %resolve-dispatched-goal (goal state environment context)
  "Validate GOAL is callable (after substitution and any module
qualification) and return (VALUES NORMALIZED-GOAL EXPLICIT-MODULE)."
  (let ((resolved-goal (logic-substitute goal environment)))
    (when (logic-var-p resolved-goal)
      (%raise-instantiation-error environment context
                                  "callable term must be instantiated"))
    (unless (or (symbolp resolved-goal)
                (%goal-form-p resolved-goal))
      (%invalid-goal resolved-goal
                     "a goal must be a symbol or a proper list headed by a symbol"))
    (let* ((qualified-p (%qualified-goal-p resolved-goal))
           (callable-goal
             (if qualified-p (third resolved-goal) resolved-goal)))
      (when (logic-var-p callable-goal)
        (%raise-instantiation-error environment context
                                    "callable term must be instantiated"))
      (unless (or (symbolp callable-goal)
                  (%goal-form-p callable-goal))
        (%invalid-goal resolved-goal
                       "a goal must be a symbol or a proper list headed by a symbol"))
      (values (%ensure-goal-form callable-goal)
              (and qualified-p
                   (%resolve-qualified-module (second resolved-goal) state))))))

(defun %prove-goal-dispatch/k (goal state succeed)
  "Prove GOAL from STATE after any active depth-limit accounting."
  (let* ((*current-table-session* (proof-state-table-session state))
         (environment (proof-state-bindings state))
         (context (%iso-atom "CALL")))
    (multiple-value-bind (normalized-goal explicit-module)
        (%resolve-dispatched-goal goal state environment context)
      (when (and (eq (first normalized-goal) (quote !))
                 (null (rest normalized-goal)))
        (funcall succeed state)
        (cl:throw (proof-state-cut-tag state) t))
      (let* ((predicate (first normalized-goal))
             (arity (length (rest normalized-goal)))
             (builtin-solver (%goal-solver predicate arity))
             (foreign-solver (%foreign-goal-solver predicate arity))
             (solver (or builtin-solver foreign-solver)))
        (cond
          (solver
            (when explicit-module
              (%find-prolog-module
               (rulebase-module-registry (proof-state-rulebase state))
               explicit-module "invoke qualified goal"))
            (let* ((solver-state
                     (if explicit-module
                         (%state-with state :module explicit-module)
                         state))
                   (*current-prolog-module* (proof-state-module solver-state))
                   (*caller-cut-tag* (proof-state-cut-tag solver-state)))
              (funcall solver
                       normalized-goal
                       (proof-state-rulebase solver-state)
                       (proof-state-bindings solver-state)
                       (proof-state-remaining-depth solver-state)
                       (lambda (bindings)
                         (funcall succeed
                                  (%state-with solver-state :bindings bindings))))))
          (t
           (multiple-value-bind (resolved-user-goal defining-module)
               (%resolve-user-goal normalized-goal state explicit-module)
             (if defining-module
                 (%prove-clauses/k resolved-user-goal
                                   (%state-with state :module defining-module)
                                   succeed)
                 (%raise-existence-error
                  "PROCEDURE" (%goal-predicate-indicator normalized-goal)
                  environment context
                  "the invoked predicate is not defined")))))))))

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

(defun %prove-raw-clauses/k (goal state succeed)
  "Prove GOAL within one predicate invocation and consume its cut.

The fresh CATCH tag is this invocation's cut barrier: a cut in any clause
body throws here, abandoning the remaining clause alternatives."
  (let* ((cut-tag (%make-cut-tag))
         (state (%state-with state :cut-tag cut-tag)))
    (cl:catch cut-tag
      (dolist (entry (%proof-predicate-entries goal state))
        (let ((clause (%stored-clause-clause entry)))
          (if (null (clause-body clause))
              (%continue-matching-fact goal clause state succeed)
              (when (%matching-rule-p goal clause)
                (%prove-rule/k goal clause state succeed))))))))

(defun %prove-rule/k (goal clause state succeed)
  "Resolve GOAL against one CLAUSE; a cut in the body prunes the clause list."
  (let ((fresh-rule (%freshen-clause clause)))
    (multiple-value-bind (extended ok)
        (unify goal (clause-head fresh-rule) (proof-state-bindings state))
      (when ok
        (flet ((prove-with-propagated-bindings (propagated)
                 (%prove-goals/k
                  (clause-body fresh-rule)
                  (%state-descending-into-rule state propagated goal)
                  succeed)))
          (if *constraint-post-unify-hook*
              (funcall *constraint-post-unify-hook*
                       extended
                       #'prove-with-propagated-bindings)
              (prove-with-propagated-bindings extended)))))))

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
