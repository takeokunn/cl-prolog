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
(defvar *call-depth-limit-token* nil)
(defvar *call-depth-limit-remaining* nil)
(defvar *call-depth-limit-used* 0)
(defvar *depth-limited-search-p* nil)
(defvar *constraint-post-unify-hook* nil
  "Function called after builtin =/2 extends an environment.")
(defvar *constraints-active-p-hook* nil
  "Function reporting whether a dynamically scoped constraint store is active.")

(define-condition %call-depth-limit-exceeded (error)
  ((token :initarg :token :reader %call-depth-limit-exceeded-token)))
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

(defun %state-with-bindings (state bindings)
  "Return STATE advanced with BINDINGS while preserving its search budget."
  (%make-proof-state (proof-state-rulebase state)
                     bindings
                     (proof-state-remaining-depth state)
                     (proof-state-module state)
                     (proof-state-table-session state)
                     (proof-state-cut-tag state)))

(defun %state-in-module (state module)
  (%make-proof-state (proof-state-rulebase state)
                     (proof-state-bindings state)
                     (proof-state-remaining-depth state)
                     module
                     (proof-state-table-session state)
                     (proof-state-cut-tag state)))

(defun %state-with-cut-tag (state cut-tag)
  "Return STATE rebased onto the cut barrier CUT-TAG."
  (if (eq (proof-state-cut-tag state) cut-tag)
      state
      (%make-proof-state (proof-state-rulebase state)
                         (proof-state-bindings state)
                         (proof-state-remaining-depth state)
                         (proof-state-module state)
                         (proof-state-table-session state)
                         cut-tag)))

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
                       (proof-state-table-session state)
                       (proof-state-cut-tag state))))

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
  (list '/ (first goal) (length (rest goal))))

(defun %clause-defines-goal-p (clause goal)
  "True when CLAUSE defines the same predicate and arity as GOAL."
  (let ((head (clause-head clause)))
    (and (consp head)
         (eq (first head) (first goal))
         (= (length (rest head)) (length (rest goal))))))

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
                 (funcall succeed (%state-with-bindings state propagated))))
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
                                         (%state-with-cut-tag next-state cut-tag)
                                         succeed))))))

(defun %prove-goal-dispatch/k (goal state succeed)
  "Prove GOAL from STATE after any active depth-limit accounting."
  (let* ((*current-table-session* (proof-state-table-session state))
         (qualified-p (%qualified-goal-p goal))
         (explicit-module (and qualified-p (second goal)))
         (normalized-goal
           (%ensure-goal-form (if qualified-p (third goal) goal))))
    (cond
      ((not (%goal-form-p normalized-goal))
       (%invalid-goal goal "a goal must be a symbol or a list headed by a symbol"))
      (t
       (when (and (eq (first normalized-goal) '!)
                  (null (rest normalized-goal)))
         ;; Deliver the current state, then prune every remaining alternative
         ;; up to the enclosing predicate invocation once search backtracks.
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
                          (%state-in-module state explicit-module)
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
                                   (%state-with-bindings solver-state bindings))))))
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

(defun %prove-goal/k (goal state succeed)
  "Prove GOAL, counting every dispatched call for local depth limits."
  (if (null *call-depth-limit-token*)
      (%prove-goal-dispatch/k goal state succeed)
      (progn
        (when (zerop *call-depth-limit-remaining*)
          (error '%call-depth-limit-exceeded
                 :token *call-depth-limit-token*))
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

(defun %prove-raw-clauses/k (goal state succeed)
  "Prove GOAL within one predicate invocation and consume its cut.

The fresh CATCH tag is this invocation's cut barrier: a cut in any clause
body throws here, abandoning the remaining clause alternatives."
  (let* ((cut-tag (%make-cut-tag))
         (state (%state-with-cut-tag state cut-tag)))
    (cl:catch cut-tag
      (dolist (entry (%proof-predicate-entries goal state))
        (let ((clause (%stored-clause-clause entry)))
          (if (null (clause-body clause))
              (%continue-matching-fact goal clause state succeed)
              (when (%matching-rule-p goal clause)
                (%prove-rule/k goal clause state succeed))))))))

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
         (resolved-goal (logic-substitute goal (proof-state-bindings state)))
         (key (list (rulebase-revision (proof-state-rulebase state))
                    (proof-state-module state)
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
