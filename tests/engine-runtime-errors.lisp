(in-package #:cl-prolog.tests)

(deftest halt-requests-processor-exit ()
  (let ((rulebase (make-rulebase)))
    (flet ((halt-code (query)
             (handler-case
                 (progn (query-prolog rulebase query) :no-halt)
               (prolog-halt (condition) (prolog-halt-code condition)))))
      (is-equal 0 (halt-code '((cl-prolog::halt))))
      (is-equal 7 (halt-code '((cl-prolog::halt 7))))
      ;; catch/3 must not intercept a halt request.
      (is-equal 2 (halt-code '((cl-prolog::catch (cl-prolog::halt 2)
                                                 ?any cl-prolog:true))))
      (signals-error (query-prolog rulebase '((cl-prolog::halt ?code))))
      (signals-error (query-prolog rulebase '((cl-prolog::halt seven)))))))

(deftest-table query-normalization-internals ()
  (:equal '() (cl-prolog::%normalize-query nil))
  (:equal '((parent tom bob))
          (cl-prolog::%normalize-query '(parent tom bob)))
  (:equal '((parent tom bob) (parent bob alice))
          (cl-prolog::%normalize-query '((parent tom bob) (parent bob alice))))
  (:equal '(!) (cl-prolog::%normalize-query '!))
  (:is-not (eq (cl-prolog::%make-cut-tag) (cl-prolog::%make-cut-tag))
           "Every cut barrier must carry a distinct catch tag"))

(deftest catch-does-not-handle-common-lisp-errors ()
  (is (handler-case
          (progn
            (query-prolog
             (make-rulebase)
             '(catch (test-programmer-error) ?error true))
            nil)
        (prolog-exception () nil)
        (simple-error () t))))

(deftest unknown-procedure-error-preserves-predicate-indicator ()
  (let* ((condition
           (capture-prolog-condition
            (lambda () (query-prolog (make-rulebase) '(missing value)))))
         (formal (second (prolog-exception-term condition))))
    (is-equal "EXISTENCE_ERROR" (symbol-name (first formal)))
    (is-equal "PROCEDURE" (symbol-name (second formal)))
    ;; Predicate indicators are ordinary compound terms: functor first.
    (destructuring-bind (slash predicate arity) (third formal)
      (is-equal "/" (symbol-name slash))
      (is-equal 'missing predicate)
      (is-equal 1 arity))))

(deftest-table goal-invocation-reports-iso-errors ()
  (:is (typep (capture-prolog-condition
               (lambda () (query-prolog (make-rulebase) '(call ?goal argument))))
              'prolog-instantiation-error))
  (:is (typep (capture-prolog-condition
               (lambda () (query-prolog (make-rulebase) '(call 42 argument))))
              'prolog-type-error))
  (:is (typep (capture-prolog-condition
               (lambda () (query-prolog (make-rulebase) '(missing value))))
              'prolog-existence-error))
  (:is (typep (capture-prolog-condition
               (lambda () (query-prolog (make-rulebase) '(not (missing)))))
              'prolog-existence-error)))

(deftest catch-handles-public-prolog-runtime-errors ()
  (let ((rb (make-rulebase)))
    (dolist (query
             '("catch(X is 1 / 0, error(evaluation_error(zero_divisor), C), true)"
               "catch(X is Y, error(instantiation_error, C), true)"
               "catch(call(42), error(type_error(callable, 42), C), true)"))
      (is (prolog-succeeds-p rb (read-prolog-term query))))
    (let* ((loop-goal (list (read-prolog-term "loop")))
           (recursive-rb
             (make-rulebase
              :clauses (list (make-clause loop-goal (list loop-goal))))))
      (is (prolog-succeeds-p
           recursive-rb
           (read-prolog-term
            "catch(loop, error(resource_error(depth_limit), C), true)")
           :max-depth 0)))))

(deftest iso-error-constructors-preserve-formal-data ()
  (dolist (case
           (list
            (list 'prolog-instantiation-error
                  (lambda ()
                    (cl-prolog::%raise-instantiation-error
                     nil (cl-prolog::%iso-atom "TEST") "test"))
                  "INSTANTIATION_ERROR")
            (list 'prolog-type-error
                  (lambda ()
                    (cl-prolog::%raise-type-error
                     "INTEGER" 'bad nil (cl-prolog::%iso-atom "TEST") "test"))
                  "TYPE_ERROR")
            (list 'prolog-domain-error
                  (lambda ()
                    (cl-prolog::%raise-domain-error
                     "NOT_LESS_THAN_ZERO" -1 nil
                     (cl-prolog::%iso-atom "TEST") "test"))
                  "DOMAIN_ERROR")
            (list 'prolog-permission-error
                  (lambda ()
                    (cl-prolog::%raise-permission-error
                     "MODIFY" "STATIC_PROCEDURE" 'test nil
                     (cl-prolog::%iso-atom "TEST") "test"))
                  "PERMISSION_ERROR")
            (list 'prolog-existence-error
                  (lambda ()
                    (cl-prolog::%raise-existence-error
                     "PROCEDURE" 'missing nil
                     (cl-prolog::%iso-atom "TEST") "test"))
                  "EXISTENCE_ERROR")
            (list 'prolog-evaluation-error
                  (lambda ()
                    (cl-prolog::%raise-evaluation-error
                     "ZERO_DIVISOR" nil (cl-prolog::%iso-atom "TEST") "test"))
                  "EVALUATION_ERROR")
            (list 'prolog-resource-error
                  (lambda ()
                    (cl-prolog::%raise-resource-error
                     "DEPTH_LIMIT" nil (cl-prolog::%iso-atom "TEST") "test"))
                  "RESOURCE_ERROR")))
    (destructuring-bind (condition-type thunk formal-name) case
      (let* ((condition (capture-prolog-condition thunk))
             (formal (second (prolog-exception-term condition))))
        (is (typep condition condition-type))
        (is-equal formal-name
                  (symbol-name (if (consp formal) (first formal) formal)))))))

(deftest-table invalid-goal-error-reports-context ()
  ;; A builtin name used at the wrong arity denotes a different, undefined
  ;; procedure (=/1), so ISO requires an existence error carrying its
  ;; predicate indicator.
  (:equal '(/ = 1)
          (handler-case
              (progn
                (query-prolog (make-rulebase) '(= a))
                (error "Expected a PROLOG-EXISTENCE-ERROR"))
            (prolog-existence-error (condition)
              (third (second (prolog-exception-term condition))))))
  (:equal '(42 x)
          (handler-case
              (progn
                (query-prolog (make-rulebase) '((42 x)))
                (error "Expected an INVALID-GOAL-ERROR"))
            (invalid-goal-error (condition)
              (invalid-goal-error-goal condition)))))
