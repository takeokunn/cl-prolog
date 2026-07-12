(in-package #:cl-prolog.tests)

(deftest-table runtime-when-guards-take-functions ()
  (:equal '(((?x . bob)))
          (query-prolog (make-family-rulebase)
                        (list 'and
                              '(parent tom ?x)
                              (list :when (lambda (x) (eq x 'bob)) '?x))))
  (:signals (query-prolog (make-family-rulebase) '(:when (equal 'bob 'bob)))))

(deftest define-foreign-predicate-registers-name-and-arity ()
  (with-macroexpansion (expansion
                        '(define-foreign-predicate (foreign-example value)
                             (rulebase environment depth emit)
                           (declare (cl:ignore rulebase depth))
                           (funcall emit environment)))
    (is (%tree-contains-p expansion 'defmethod))
    (is (%tree-contains-p expansion 'cl-prolog::%foreign-goal-solver))
    (is (%tree-contains-p expansion 'foreign-example))
    (is (%tree-contains-p expansion 1))))

(deftest registered-foreign-predicate-is-authoritative ()
  (let ((rb (prolog ((foreign-choice clause-solution)))))
    (assert-query rb (foreign-choice ?value)
                  => (((?value . left)) ((?value . right))))))

(deftest foreign-predicate-dispatches-by-exact-indicator ()
  (let ((rb (prolog
              ((foreign-choice fallback zero))
              ((foreign-choice fallback)))))
    ;; No FOREIGN-CHOICE/0 solver or clause exists, so ISO requires an
    ;; existence error rather than a silent failure.
    (assert-query rb (foreign-choice) :signals)
    (assert-query rb (foreign-choice fallback zero) => (nil))
    (assert-query rb (foreign-choice fallback) => ())))

(deftest foreign-predicate-cps-solutions ()
  (let ((rb (make-family-rulebase))
        (*recorded-colors* '()))
    (assert-query rb (accepted :ok) => (nil))
    (assert-query rb (accepted :ng) => ())
    (assert-query rb (colored ?x) => (((?x . tom))))
    (is-equal '(tom) *recorded-colors*)
    (assert-query rb (foreign-choice ?value)
                  => (((?value . left)) ((?value . right))))))
