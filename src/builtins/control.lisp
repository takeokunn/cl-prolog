(in-package #:cl-prolog)

;;; Control builtins

(define-builtin (true) (rulebase environment depth emit)
  (funcall emit environment))

(define-builtin ((fail false)) (rulebase environment depth emit)
  (declare (ignore rulebase environment depth emit)))

(define-builtin (!) (rulebase environment depth emit)
  (funcall emit environment)
  (%propagate-cut))

(define-builtin (= left right) (rulebase environment depth emit)
  (%unify-emit left right environment emit))

(define-builtin ((/= !=) left right) (rulebase environment depth emit)
  (unless (nth-value 1 (unify left right environment))
    (funcall emit environment)))

(define-builtin ((not |\+|) goal) (rulebase environment depth emit)
  (unless (%provable-p (logic-substitute goal environment)
                       rulebase environment depth)
    (funcall emit environment)))

(defun %extend-callable-goal (closure arguments)
  "Append ARGUMENTS to CLOSURE, returning an engine-level goal form."
  (cond
    ((logic-var-p closure)
     (%invalid-goal closure "CALL/N requires an instantiated callable term"))
    ((symbolp closure)
     (cons closure arguments))
    ((and (consp closure) (symbolp (first closure)))
     (append closure arguments))
    (t
     (%invalid-goal closure "CALL/N requires a callable atom or compound term"))))

(define-builtin (call closure &rest arguments) (rulebase environment depth emit)
  (let ((resolved-closure (logic-substitute closure environment))
        (resolved-arguments
          (mapcar (lambda (argument)
                    (logic-substitute argument environment))
                  arguments)))
    (%prove-bindings/k
     (%extend-callable-goal resolved-closure resolved-arguments)
     rulebase environment depth emit)))

(define-builtin (once goal) (rulebase environment depth emit)
  (block first-proof
    (%prove-bindings/k
     (logic-substitute goal environment)
     rulebase environment depth
     (lambda (extended)
       (funcall emit extended)
       (return-from first-proof nil)))))

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

(define-builtin (if-then-else condition then else)
    (rulebase environment depth emit)
  (multiple-value-bind (condition-environment matched-p)
      (%first-proof-environment condition rulebase environment depth)
    (%prove-bindings/k
     (logic-substitute (if matched-p then else)
                       (if matched-p condition-environment environment))
     rulebase
     (if matched-p condition-environment environment)
     depth
     emit)))

(define-builtin (soft-if-then-else condition then else)
    (rulebase environment depth emit)
  (let ((matched-p nil))
    (%prove-bindings/k
     (logic-substitute condition environment)
     rulebase environment depth
     (lambda (condition-environment)
       (setf matched-p t)
       (%prove-bindings/k
        (logic-substitute then condition-environment)
        rulebase condition-environment depth emit)))
    (unless matched-p
      (%prove-bindings/k
       (logic-substitute else environment)
       rulebase environment depth emit))))

(define-builtin (throw ball) (rulebase environment depth emit)
  (declare (ignore rulebase depth emit))
  (%raise-prolog-exception (logic-substitute ball environment) environment))

(define-builtin (catch goal catcher recover) (rulebase environment depth emit)
  (handler-case
      (%prove-bindings/k
       (logic-substitute goal environment)
       rulebase environment depth emit)
    (prolog-exception (condition)
      (let ((thrown-environment
              (or (%prolog-exception-environment condition) environment)))
        (multiple-value-bind (recovery-environment matched-p)
            (unify (prolog-exception-term condition)
                   (logic-substitute catcher thrown-environment)
                   thrown-environment)
          (if matched-p
              (%prove-bindings/k
               (logic-substitute recover recovery-environment)
               rulebase recovery-environment depth emit)
              (error condition)))))))

(define-builtin (repeat) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (loop (funcall emit environment)))

(define-builtin (and &rest goals) (rulebase environment depth emit)
  (when (%prove-bindings/k goals rulebase environment depth emit)
    (%propagate-cut)))

(define-builtin (or &rest alternatives) (rulebase environment depth emit)
  (dolist (alternative alternatives)
    (when (%prove-bindings/k alternative rulebase environment depth emit)
      (%propagate-cut))))
