;;;; Tabling (memoized resolution): left-recursion detection and the
;;;; declared-tabled-predicate answer cache built on top of core CPS
;;;; proof search.

(in-package #:cl-prolog)

(defun %replay-table-answers/k (goal state entry succeed)
  "Unify each stored answer for ENTRY with GOAL and invoke SUCCEED."
  (loop repeat (%table-entry-answer-count entry)
        for answer in (%table-entry-answers entry)
        do (multiple-value-bind (extended ok)
               (unify goal (%instantiate-variant answer)
                      (proof-state-bindings state))
             (when ok
               (funcall succeed (%state-with state :bindings extended))))))

(defun %predicate-key (goal)
  (when (%goal-form-p goal)
    (cons (first goal) (length (rest goal)))))

(defparameter +tabling-transparent-control-strategies+
  '(("NOT" . :unary) ("\\+" . :unary) ("ONCE" . :unary) ("IGNORE" . :unary)
    ("CALL_NTH" . :unary) ("CALL_WITH_DEPTH_LIMIT" . :unary)
    ("AND" . :sequence) ("SETUP_CALL_CLEANUP" . :sequence)
    ("CALL_CLEANUP" . :sequence) ("FORALL" . :sequence)
    ("OR" . :alternatives)
    ("IF-THEN-ELSE" . :if-then-else) ("SOFT-IF-THEN-ELSE" . :if-then-else)
    ("CATCH" . :catch))
  "How %FIRST-USER-PREDICATE-KEYS's left-recursion analysis sees through a
control construct to the first-user-goal(s) beyond it, keyed by ISO functor
name: :UNARY looks only at the second argument; :SEQUENCE and :ALTERNATIVES
look at every remaining argument, conjunctively or disjunctively
transparent; :IF-THEN-ELSE and :CATCH thread a distinguished condition goal
through their branches. CALL is handled separately -- it resolves its
closure argument before recursing, rather than picking a fixed argument
shape.")

(defun %first-user-predicate-keys (clause)
  "Return possible first user-predicate indicators reached by CLAUSE."
  (labels ((static-call-goal (closure arguments)
             (cond
               ((and (symbolp closure)
                     (not (logic-var-p closure)))
                (cons closure arguments))
               ((and (consp closure) (%goal-form-p closure))
                (append closure arguments))))
           (analyze-alternatives (goals)
             (let ((keys '())
                   (transparent-p nil))
               (dolist (goal goals)
                 (multiple-value-bind (goal-keys goal-transparent-p)
                     (analyze-goal goal)
                   (setf keys (nconc keys goal-keys)
                         transparent-p
                         (or transparent-p goal-transparent-p))))
               (values (remove-duplicates keys :test #'equal)
                       transparent-p)))
           (analyze-sequence (goals)
             (if (null goals)
                 (values nil t)
                 (multiple-value-bind (keys transparent-p)
                     (analyze-goal (first goals))
                   (if transparent-p
                       (multiple-value-bind (later-keys later-transparent-p)
                           (analyze-sequence (rest goals))
                         (values (remove-duplicates
                                  (nconc keys later-keys) :test #'equal)
                                 later-transparent-p))
                       (values keys nil)))))
           (analyze-conditional (condition branches)
             (multiple-value-bind (keys transparent-p)
                 (analyze-goal condition)
               (if transparent-p
                   (multiple-value-bind (branch-keys branch-transparent-p)
                       (analyze-alternatives branches)
                     (values (remove-duplicates
                              (nconc keys branch-keys) :test #'equal)
                             branch-transparent-p))
                   (values keys nil))))
           (analyze-goal (raw-goal)
             (let* ((goal (%ensure-goal-form raw-goal))
                    (key (%predicate-key goal)))
               (cond
                 ((null key) (values nil t))
                 ((and (not (%goal-solver (car key) (cdr key)))
                       (not (%foreign-goal-solver (car key) (cdr key))))
                  (values (list key) nil))
                 ((%foreign-goal-solver (car key) (cdr key))
                  (values nil t))
                 (t
                  (let ((name (string-upcase (symbol-name (first goal)))))
                    (if (string= name "CALL")
                        (let ((called (static-call-goal (second goal)
                                                        (cddr goal))))
                          (if called
                              (analyze-goal called)
                              (values nil t)))
                        (case (cdr (assoc name
                                          +tabling-transparent-control-strategies+
                                          :test #'string=))
                          (:unary (analyze-goal (second goal)))
                          (:sequence (analyze-sequence (rest goal)))
                          (:alternatives (analyze-alternatives (rest goal)))
                          (:if-then-else
                           (analyze-conditional (second goal) (cddr goal)))
                          (:catch
                           (analyze-conditional (second goal)
                                                (list (fourth goal))))
                          (otherwise (values nil t))))))))))
    (nth-value 0 (analyze-sequence (clause-body clause)))))

(defun %first-user-goal-adjacency (state)
  "Return (VALUES ADJACENCY REVERSE-ADJACENCY NODES) for the call graph
STATE's rulebase forms among first-user-goal predicate keys."
  (let ((adjacency (make-hash-table :test #'equal))
        (reverse-adjacency (make-hash-table :test #'equal))
        (nodes '()))
    (labels ((ensure-node (key)
               (multiple-value-bind (neighbors node-present-p)
                   (gethash key adjacency)
                 (declare (ignore neighbors))
                 (unless node-present-p
                   (setf (gethash key adjacency) '()
                         (gethash key reverse-adjacency) '())
                   (push key nodes)))))
      (dolist (entry (%proof-module-entries state))
        (let* ((clause (%stored-clause-clause entry))
               (head-key (%predicate-key (clause-head clause)))
               (successors (%first-user-predicate-keys clause)))
          (when head-key
            (ensure-node head-key)
            (dolist (successor successors)
              (ensure-node successor)
              (push successor (gethash head-key adjacency))
              (push head-key (gethash successor reverse-adjacency)))))))
    (values adjacency reverse-adjacency nodes)))

(defun %dfs-finish-order (nodes adjacency)
  "Return NODES in iterative depth-first postorder over ADJACENCY
(Kosaraju's algorithm, first pass)."
  (let ((visited (make-hash-table :test #'equal))
        (finish-order '()))
    (dolist (node nodes)
      (unless (gethash node visited)
        (let ((stack (list (cons node nil))))
          (loop while stack
                for frame = (pop stack)
                for current = (car frame)
                for expanded-p = (cdr frame)
                do (if expanded-p
                       (push current finish-order)
                       (unless (gethash current visited)
                         (setf (gethash current visited) t)
                         (push (cons current t) stack)
                         (dolist (next (gethash current adjacency))
                           (unless (gethash next visited)
                             (push (cons next nil) stack)))))))))
    finish-order))

(defun %strongly-connected-recursive-nodes
    (finish-order adjacency reverse-adjacency)
  "Return a hash-table marking every node in FINISH-ORDER that belongs to a
nontrivial strongly-connected component or has a self-loop, over ADJACENCY
and its transpose REVERSE-ADJACENCY (Kosaraju's algorithm, second pass)."
  (let ((assigned (make-hash-table :test #'equal))
        (recursive (make-hash-table :test #'equal)))
    (dolist (node finish-order)
      (unless (gethash node assigned)
        (let ((component '())
              (stack (list node)))
          (loop while stack
                for current = (pop stack)
                do (unless (gethash current assigned)
                     (setf (gethash current assigned) t)
                     (push current component)
                     (dolist (previous (gethash current reverse-adjacency))
                       (unless (gethash previous assigned)
                         (push previous stack)))))
          (when (or (rest component)
                    (member (first component)
                            (gethash (first component) adjacency)
                            :test #'equal))
            (dolist (member component)
              (setf (gethash member recursive) t))))))
    recursive))

(defun %left-recursive-p (goal state)
  "Return true when GOAL belongs to a first-user-goal call cycle."
  (let* ((target (%predicate-key goal))
         (rulebase (proof-state-rulebase state))
         (module (proof-state-module state))
         (cache (%table-session-left-recursion
                 (proof-state-table-session state)))
         (cache-key (list (rulebase-revision rulebase) module)))
    (when target
      (multiple-value-bind (index present-p) (gethash cache-key cache)
        (unless present-p
          (multiple-value-bind (adjacency reverse-adjacency nodes)
              (%first-user-goal-adjacency state)
            (setf index (%strongly-connected-recursive-nodes
                         (%dfs-finish-order nodes adjacency)
                         adjacency reverse-adjacency)
                  (gethash cache-key cache) index)))
        (not (null (gethash target index)))))))

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
             (resolved-goal
               (logic-substitute goal (proof-state-bindings state))))
        (multiple-value-bind (canonical-goal cyclic-goal-p)
            (%canonicalize-variant resolved-goal)
          (let* ((key
                   (if cyclic-goal-p
                       (list (rulebase-revision
                              (proof-state-rulebase state))
                             (proof-state-module state)
                             :cyclic
                             (%variant-graph-key canonical-goal))
                       (list (rulebase-revision
                              (proof-state-rulebase state))
                             (proof-state-module state)
                             canonical-goal)))
                 (entries (%table-session-entries session))
                 (entry (gethash key entries)))
            (if entry
                (%replay-table-answers/k goal state entry succeed)
                (let ((entry (%make-table-entry))
                      (completed-p nil))
                  (labels ((record-answer-p (answer cyclic-p)
                             (let* ((index
                                      (if cyclic-p
                                          (%table-entry-cyclic-answer-index
                                           entry)
                                          (%table-entry-answer-index entry)))
                                    (answer-key
                                      (if cyclic-p
                                          (%variant-graph-key answer)
                                          answer)))
                               (multiple-value-bind (stored present-p)
                                   (gethash answer-key index)
                                 (declare (ignore stored))
                                 (unless present-p
                                   (let ((cell (list answer))
                                         (tail
                                           (%table-entry-answers-tail entry)))
                                     (if tail
                                         (setf (cdr tail) cell)
                                         (setf (%table-entry-answers entry)
                                               cell))
                                     (setf (%table-entry-answers-tail entry)
                                           cell
                                           (gethash answer-key index) t)
                                     (incf (%table-entry-answer-count entry))
                                     t))))))
                    (setf (gethash key entries) entry)
                    (unwind-protect
                         (progn
                           (loop
                             with changed-p
                             do (setf changed-p nil)
                                (%prove-raw-clauses/k
                                 goal state
                                 (lambda (answer-state)
                                   (multiple-value-bind
                                         (answer cyclic-answer-p)
                                       (%canonicalize-variant
                                        (logic-substitute
                                         goal
                                         (proof-state-bindings
                                          answer-state)))
                                     (when (record-answer-p
                                            answer cyclic-answer-p)
                                       (setf changed-p t)
                                       (funcall succeed answer-state)))))
                             while changed-p)
                           (setf completed-p t))
                      (unless completed-p
                        (remhash key entries)))))))))))
