(in-package #:cl-prolog)

;;; Control builtins

(define-builtin (true) (rulebase environment depth emit)
  (funcall emit environment))

(define-builtin ((fail false)) (rulebase environment depth emit)
  (declare (cl:ignore rulebase environment depth emit)))

(define-builtin (= left right) (rulebase environment depth emit)
  (%unify-emit left right environment emit))

(define-builtin (unify_with_occurs_check left right)
    (rulebase environment depth emit)
  ;; UNIFY always performs the occurs check.
  (%unify-emit left right environment emit))

(define-builtin (|\\=| left right) (rulebase environment depth emit)
  (unless (nth-value 1 (unify left right environment))
    (funcall emit environment)))

(define-builtin ((not |\\+|) goal) (rulebase environment depth emit)
  (unless (%provable-p (logic-substitute goal environment)
                       rulebase environment depth)
    (funcall emit environment)))

(defun %extend-callable-goal (closure arguments environment)
  "Append ARGUMENTS to CLOSURE, returning an engine-level goal form."
  (cond
    ((logic-var-p closure)
     (%raise-instantiation-error environment (%iso-atom "CALL")
                                 "CALL/N requires an instantiated callable term"))
    ((symbolp closure)
     (cons closure arguments))
    ((and (consp closure) (symbolp (first closure)))
     (append closure arguments))
    (t
     (%raise-type-error "CALLABLE" closure environment (%iso-atom "CALL")
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
     (logic-substitute goal environment)
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

(defun %call-cleanup/k (setup goal cleanup rulebase environment depth emit)
  "Commit to SETUP's first proof, then run CLEANUP exactly once after GOAL."
  (multiple-value-bind (setup-environment setup-succeeded-p)
      (%first-proof-environment setup rulebase environment depth)
    (when setup-succeeded-p
      (let ((cleanup-environment setup-environment)
            (cleanup-ran-p nil))
        (labels ((run-cleanup ()
                   (unless cleanup-ran-p
                     (setf cleanup-ran-p t)
                     (%first-proof-environment cleanup
                                               rulebase cleanup-environment
                                               depth))))
          (unwind-protect
               (%prove-bindings/k
                (logic-substitute goal setup-environment)
                rulebase setup-environment depth
                (lambda (goal-environment)
                  (setf cleanup-environment goal-environment)
                  (run-cleanup)
                  (funcall emit goal-environment)))
            (run-cleanup)))))))

(define-builtin (setup_call_cleanup setup goal cleanup)
    (rulebase environment depth emit)
  (%call-cleanup/k setup goal cleanup
                   rulebase environment depth emit))

(define-builtin (call_cleanup goal cleanup)
    (rulebase environment depth emit)
  (%call-cleanup/k 'true goal cleanup
                   rulebase environment depth emit))

(define-builtin (forall condition action) (rulebase environment depth emit)
  (let ((succeeded-p t))
    (block failed-action
      (%prove-bindings/k
       (logic-substitute condition environment)
       rulebase environment depth
       (lambda (condition-environment)
         (unless (nth-value 1
                            (%first-proof-environment action rulebase
                                                      condition-environment
                                                      depth))
           (setf succeeded-p nil)
           (return-from failed-action)))))
    (when succeeded-p
      (funcall emit environment))))

(define-builtin (cl-prolog.user-atoms::ignore goal)
    (rulebase environment depth emit)
  (multiple-value-bind (goal-environment succeeded-p)
      (%first-proof-environment goal rulebase environment depth)
    (funcall emit (if succeeded-p goal-environment environment))))

(define-builtin (if-then-else condition then else)
    (rulebase environment depth emit)
  ;; The condition is opaque to cut (ONCE semantics); the taken branch
  ;; shares the caller's cut barrier, as ISO requires.
  (let ((caller-cut-tag *caller-cut-tag*))
    (multiple-value-bind (condition-environment matched-p)
        (%first-proof-environment condition rulebase environment depth)
      (let ((branch-environment
              (if matched-p condition-environment environment)))
        (%prove-with-cut-tag/k
         (logic-substitute (if matched-p then else) branch-environment)
         rulebase branch-environment depth caller-cut-tag emit)))))

(define-builtin (soft-if-then-else condition then else)
    (rulebase environment depth emit)
  ;; Unlike IF-THEN-ELSE the condition keeps all its solutions; the taken
  ;; branch still shares the caller's cut barrier.
  (let ((caller-cut-tag *caller-cut-tag*)
        (matched-p nil))
    (%prove-bindings/k
     (logic-substitute condition environment)
     rulebase environment depth
     (lambda (condition-environment)
       (setf matched-p t)
       (%prove-with-cut-tag/k
        (logic-substitute then condition-environment)
        rulebase condition-environment depth caller-cut-tag emit)))
    (unless matched-p
      (%prove-with-cut-tag/k
       (logic-substitute else environment)
       rulebase environment depth caller-cut-tag emit))))

(define-builtin (throw ball) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth emit))
  (let ((resolved-ball (logic-substitute ball environment)))
    (when (logic-var-p resolved-ball)
      (%raise-instantiation-error environment (%iso-atom "THROW")
                                  "THROW/1 requires a non-variable ball"))
    (%raise-prolog-exception resolved-ball environment)))

(define-builtin (catch goal catcher recover) (rulebase environment depth emit)
  (handler-case
      (%prove-bindings/k
       (logic-substitute goal environment)
       rulebase environment depth emit)
    (prolog-exception (condition)
      (multiple-value-bind (recovery-environment matched-p)
          (unify (prolog-exception-term condition)
                 (logic-substitute catcher environment)
                 environment)
        (if matched-p
            (%prove-bindings/k
             (logic-substitute recover recovery-environment)
             rulebase recovery-environment depth emit)
            (error condition))))))

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
