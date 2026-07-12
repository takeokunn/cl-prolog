(in-package #:cl-prolog)

;;; Control builtins

(define-builtin (true) (rulebase environment depth emit)
  (funcall emit environment))

(define-builtin ((fail false)) (rulebase environment depth emit)
  (declare (cl:ignore rulebase environment depth emit)))

(define-builtin (= left right) (rulebase environment depth emit)
  (%constraint-unify-emit left right environment emit))

(define-builtin (unify_with_occurs_check left right)
    (rulebase environment depth emit)
  ;; UNIFY always performs the occurs check.
  (%unify-emit left right environment emit))

(define-builtin (|\\=| left right) (rulebase environment depth emit)
  (unless (nth-value 1 (unify left right environment))
    (funcall emit environment)))

(defun %resolve-callable-goal (goal environment operation)
  "Resolve GOAL in ENVIRONMENT and validate it as an ISO callable term."
  (%ensure-callable (logic-substitute goal environment)
                    environment operation))

(define-builtin ((not |\\+|) goal) (rulebase environment depth emit)
  (unless (%provable-p (%resolve-callable-goal goal environment
                                               (%iso-atom "NOT"))
                       rulebase environment depth *current-prolog-module*)
    (funcall emit environment)))

(defun %extend-callable-goal (closure arguments environment
                              &optional (operation (%iso-atom "CALL")))
  "Append ARGUMENTS to CLOSURE, returning an engine-level goal form."
  (cond
    ((logic-var-p closure)
     (%raise-instantiation-error environment operation
                                 "CALL/N requires an instantiated callable term"))
    ((symbolp closure)
     (cons closure arguments))
    ((and (consp closure) (symbolp (first closure)))
     (append closure arguments))
    (t
     (%raise-type-error "CALLABLE" closure environment operation
                        "CALL/N requires a callable atom or compound term"))))

(define-builtin (call closure &rest arguments) (rulebase environment depth emit)
  (let ((resolved-closure (logic-substitute closure environment))
        (resolved-arguments
          (mapcar (lambda (argument)
                    (logic-substitute argument environment))
                  arguments)))
    (%prove-bindings/k
     (%extend-callable-goal resolved-closure resolved-arguments environment)
     rulebase environment depth emit)))

(define-builtin (once goal) (rulebase environment depth emit)
  (block first-proof
    (%prove-bindings/k
     (%resolve-callable-goal goal environment (%iso-atom "ONCE"))
     rulebase environment depth
     (lambda (extended)
       (funcall emit extended)
       (return-from first-proof nil)))))

(define-builtin (call_nth goal n) (rulebase environment depth emit)
  (let* ((resolved-goal
           (%extend-callable-goal (logic-substitute goal environment)
                                  '() environment))
         (resolved-n (logic-substitute n environment)))
    (unless (or (logic-var-p resolved-n) (integerp resolved-n))
      (%raise-type-error "INTEGER" resolved-n environment
                         (%iso-atom "CALL_NTH")
                         "call_nth/2 requires an integer solution number"))
    (when (and (integerp resolved-n) (< resolved-n 1))
      (%raise-domain-error "NOT_LESS_THAN_ONE" resolved-n environment
                           (%iso-atom "CALL_NTH")
                           "call_nth/2 requires a positive solution number"))
    (let ((count 0))
      (block requested-proof
        (%prove-bindings/k
         resolved-goal rulebase environment depth
         (lambda (extended)
           (incf count)
           (if (logic-var-p resolved-n)
               (%unify-emit n count extended emit)
               (when (= count resolved-n)
                 (funcall emit extended)
                 (return-from requested-proof nil)))))))))

(define-builtin (call_with_depth_limit goal limit result)
    (rulebase environment depth emit)
  (let* ((operation (%iso-atom "CALL_WITH_DEPTH_LIMIT"))
         (resolved-goal
           (%extend-callable-goal (logic-substitute goal environment)
                                  '() environment operation))
         (resolved-limit (logic-substitute limit environment)))
    (when (logic-var-p resolved-limit)
      (%raise-instantiation-error
       environment operation
       "call_with_depth_limit/3 requires an instantiated depth limit"))
    (unless (integerp resolved-limit)
      (%raise-type-error "INTEGER" resolved-limit environment operation
                         "call_with_depth_limit/3 requires an integer depth limit"))
    (when (minusp resolved-limit)
      (%raise-domain-error
       "NOT_LESS_THAN_ZERO" resolved-limit environment operation
       "call_with_depth_limit/3 requires a non-negative depth limit"))
    (let ((token (list '%call-depth-limit))
          (outer-token *call-depth-limit-token*)
          (outer-remaining *call-depth-limit-remaining*)
          (outer-used *call-depth-limit-used*)
          (outer-depth-limited-p *depth-limited-search-p*))
      (let ((*call-depth-limit-token* token)
            (*call-depth-limit-remaining* resolved-limit)
            (*call-depth-limit-used* 0)
            (*depth-limited-search-p* t))
        (when (eq token
                  (cl:catch token
                    (%prove-bindings/k
                     resolved-goal rulebase environment depth
                     (lambda (extended)
                       (let ((used *call-depth-limit-used*)
                             (*call-depth-limit-token* outer-token)
                             (*call-depth-limit-remaining* outer-remaining)
                             (*call-depth-limit-used* outer-used)
                             (*depth-limited-search-p* outer-depth-limited-p))
                         (%unify-emit result used extended emit))))
                    nil))
          (let ((*call-depth-limit-token* outer-token)
                (*call-depth-limit-remaining* outer-remaining)
                (*call-depth-limit-used* outer-used)
                (*depth-limited-search-p* outer-depth-limited-p))
            (%unify-emit result (%iso-atom "DEPTH_LIMIT_EXCEEDED")
                         environment emit)))))))

(defun %first-proof-environment (goal rulebase environment depth)
  "Return the first proof environment for GOAL and whether one exists."
  (let ((matched-p nil)
        (result nil))
    (block first-proof
      (%prove-bindings/k
       (logic-substitute goal environment)
       rulebase environment depth
       (lambda (extended)
         (setf matched-p t
               result extended)
         (return-from first-proof))))
    (values result matched-p)))

(defun %cleanup-goal-deterministic-p (goal)
  "Return true when GOAL is a builtin that cannot yield multiple proofs."
  (let ((name (and (consp goal)
                   (symbolp (first goal))
                   (symbol-name (first goal)))))
    (or (and (symbolp goal)
             (string= (symbol-name goal) "TRUE"))
        (member name '("TRUE" "=" "UNIFY_WITH_OCCURS_CHECK" "==" "\\==")
                :test #'string=))))

(defun %call-cleanup/k (setup goal cleanup rulebase environment depth emit
                        operation)
  "Commit to SETUP, stream GOAL's proofs, then run CLEANUP exactly once.

CLEANUP failure is ignored, while a condition raised by CLEANUP propagates."
  (multiple-value-bind (setup-environment setup-succeeded-p)
      (%first-proof-environment
       (%resolve-callable-goal setup environment operation)
       rulebase environment depth)
    (when setup-succeeded-p
      (let ((cleanup-environment setup-environment)
            (cleanup-ran-p nil)
            (deterministic-goal-p (%cleanup-goal-deterministic-p goal)))
        (labels ((run-cleanup ()
                   (unless cleanup-ran-p
                     (setf cleanup-ran-p t)
                     (%first-proof-environment
                      (%resolve-callable-goal cleanup cleanup-environment
                                              operation)
                      rulebase cleanup-environment depth))))
          (unwind-protect
               (%prove-bindings/k
                (%resolve-callable-goal goal setup-environment operation)
                rulebase setup-environment depth
                (lambda (goal-environment)
                  (setf cleanup-environment goal-environment)
                  (when deterministic-goal-p
                    (run-cleanup))
                  (funcall emit goal-environment)))
            (run-cleanup)))))))

(define-builtin (setup_call_cleanup setup goal cleanup)
    (rulebase environment depth emit)
  (%call-cleanup/k setup goal cleanup
                   rulebase environment depth emit
                   (%iso-atom "SETUP_CALL_CLEANUP")))

(define-builtin (call_cleanup goal cleanup)
    (rulebase environment depth emit)
  (%call-cleanup/k 'true goal cleanup
                   rulebase environment depth emit
                   (%iso-atom "CALL_CLEANUP")))

(define-builtin (forall condition action) (rulebase environment depth emit)
  (let ((succeeded-p t))
    (block failed-action
      (%prove-bindings/k
       (%resolve-callable-goal condition environment (%iso-atom "FORALL"))
       rulebase environment depth
       (lambda (condition-environment)
         (unless (nth-value 1
                            (%first-proof-environment
                             (%resolve-callable-goal
                              action condition-environment
                              (%iso-atom "FORALL"))
                             rulebase condition-environment depth))
           (setf succeeded-p nil)
           (return-from failed-action)))))
    (when succeeded-p
      (funcall emit environment))))

(define-builtin (cl-prolog.user-atoms::ignore goal)
    (rulebase environment depth emit)
  (multiple-value-bind (goal-environment succeeded-p)
      (%first-proof-environment
       (%resolve-callable-goal goal environment (%iso-atom "IGNORE"))
       rulebase environment depth)
    (funcall emit (if succeeded-p goal-environment environment))))

(define-builtin (if-then-else condition then else)
    (rulebase environment depth emit)
  ;; The condition is opaque to cut (ONCE semantics); the taken branch
  ;; shares the caller's cut barrier, as ISO requires.
  (let ((caller-cut-tag *caller-cut-tag*))
    (multiple-value-bind (condition-environment matched-p)
        (%first-proof-environment
         (%resolve-callable-goal condition environment
                                 (%iso-atom "IF_THEN_ELSE"))
         rulebase environment depth)
      (let ((branch-environment
              (if matched-p condition-environment environment)))
        (%prove-with-cut-tag/k
         (%resolve-callable-goal (if matched-p then else)
                                 branch-environment
                                 (%iso-atom "IF_THEN_ELSE"))
         rulebase branch-environment depth caller-cut-tag emit)))))

(define-builtin (soft-if-then-else condition then else)
    (rulebase environment depth emit)
  ;; Unlike IF-THEN-ELSE the condition keeps all its solutions; the taken
  ;; branch still shares the caller's cut barrier.
  (let ((caller-cut-tag *caller-cut-tag*)
        (matched-p nil))
    (%prove-bindings/k
     (%resolve-callable-goal condition environment
                             (%iso-atom "SOFT_IF_THEN_ELSE"))
     rulebase environment depth
     (lambda (condition-environment)
       (setf matched-p t)
       (%prove-with-cut-tag/k
        (%resolve-callable-goal then condition-environment
                                (%iso-atom "SOFT_IF_THEN_ELSE"))
        rulebase condition-environment depth caller-cut-tag emit)))
    (unless matched-p
      (%prove-with-cut-tag/k
       (%resolve-callable-goal else environment
                               (%iso-atom "SOFT_IF_THEN_ELSE"))
       rulebase environment depth caller-cut-tag emit))))

(define-builtin (throw ball) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth emit))
  (let ((resolved-ball (logic-substitute ball environment)))
    (when (logic-var-p resolved-ball)
      (%raise-instantiation-error environment (%iso-atom "THROW")
                                  "THROW/1 requires a non-variable ball"))
    (%raise-prolog-exception resolved-ball environment)))

(define-builtin (catch goal catcher recover) (rulebase environment depth emit)
  (let ((continuation-condition nil))
    (handler-case
        (%prove-bindings/k
         (%resolve-callable-goal goal environment (%iso-atom "CATCH"))
         rulebase environment depth
         (lambda (goal-environment)
           (handler-case
               (funcall emit goal-environment)
             (prolog-exception (condition)
               (setf continuation-condition condition)
               (error condition)))))
      (prolog-exception (condition)
        (when (eq condition continuation-condition)
          (error condition))
        (multiple-value-bind (recovery-environment matched-p)
            (unify (prolog-exception-term condition)
                   (logic-substitute catcher environment)
                   environment)
          (if matched-p
              (%prove-bindings/k
               (%resolve-callable-goal recover recovery-environment
                                       (%iso-atom "CATCH"))
               rulebase recovery-environment depth emit)
              (error condition)))))))

(define-builtin (repeat) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (loop (funcall emit environment)))

(define-builtin (halt) (rulebase environment depth emit)
  (declare (cl:ignore rulebase environment depth emit))
  (error 'prolog-halt :code 0))

(define-builtin (halt code) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth emit))
  (let ((resolved (logic-substitute code environment)))
    (when (logic-var-p resolved)
      (%raise-instantiation-error environment (%iso-atom "HALT")
                                  "halt/1 requires an instantiated exit code"))
    (unless (integerp resolved)
      (%raise-type-error "INTEGER" resolved environment (%iso-atom "HALT")
                         "halt/1 requires an integer exit code"))
    (error 'prolog-halt :code resolved)))

(define-builtin (and &rest goals) (rulebase environment depth emit)
  ;; GOALS is always a list of goals; normalize each element so a leading
  ;; bare atom is not mistaken for a compound goal's functor.
  (%prove-transparent/k (mapcar #'%ensure-goal-form goals)
                        rulebase environment depth emit))

(define-builtin (or &rest alternatives) (rulebase environment depth emit)
  (let ((caller-cut-tag *caller-cut-tag*))
    (dolist (alternative alternatives)
      (%prove-with-cut-tag/k alternative rulebase environment depth
                             caller-cut-tag emit))))
