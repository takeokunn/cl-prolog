(in-package #:cl-prolog)

;;; Control builtins

(define-builtin (true) (rulebase environment depth emit)
  (funcall emit environment))

(define-builtin (!) (rulebase environment depth emit)
  (funcall emit environment)
  (%propagate-cut))

(define-builtin (= left right) (rulebase environment depth emit)
  (%unify-emit left right environment emit))

(define-builtin ((/= !=) left right) (rulebase environment depth emit)
  (unless (nth-value 1 (unify left right environment))
    (funcall emit environment)))

(define-builtin (not goal) (rulebase environment depth emit)
  (unless (%provable-p goal rulebase environment (1- depth))
    (funcall emit environment)))

(define-builtin (call goal) (rulebase environment depth emit)
  (%prove-bindings/k
   (logic-substitute goal environment)
   rulebase environment (1- depth) emit))

(define-builtin (once goal) (rulebase environment depth emit)
  (block first-proof
    (%prove-bindings/k
     (logic-substitute goal environment)
     rulebase environment (1- depth)
     (lambda (extended)
       (funcall emit extended)
       (return-from first-proof nil)))))

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
