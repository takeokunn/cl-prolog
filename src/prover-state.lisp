(in-package #:cl-prolog)

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
