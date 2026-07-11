;;;; Query expectation helpers.

(in-package #:cl-prolog.tests)

(defun %query-run-form (rulebase query kind options)
  "Return the engine call used to evaluate QUERY for KIND."
  (ecase kind
    ((:first)
     `(query-prolog-first ,rulebase ',query ,@options))
    ((=> :set :signals :succeeds :fails)
     `(query-prolog ,rulebase ',query ,@options))))

(defun %query-spec-assertion (spec)
  "Compile one (QUERY KIND EXPECTED... OPTIONS...) spec into an assertion form.

The predicate kinds (:succeeds, :fails, :signals) take no EXPECTED value, so any
trailing tokens are OPTIONS (e.g. :max-depth 16).  This mirrors ASSERT-QUERY."
  (destructuring-bind (query kind &rest rest) spec
    (multiple-value-bind (expected options)
        (case kind
          ((:succeeds :fails :signals) (values nil rest))
          (otherwise (values (first rest) (rest rest))))
      (let ((run (%query-run-form '%rulebase query kind options)))
        (ecase kind
          (=>        `(is-equal ',expected ,run))
          (:set      `(is-same-set ',expected ,run))
          (:first    `(is-equal ',expected ,run))
          (:succeeds `(is (not (null ,run))))
          (:fails    `(is (null ,run)))
          (:signals  `(signals-error ,run)))))))

(defmacro deftest-queries (name (rulebase-form) &body specs)
  "Define a test that checks each query expectation SPEC against RULEBASE-FORM.

SPEC is (QUERY KIND EXPECTED... OPTIONS...):
  (Q => SOLUTIONS)    ordered solution list equality
  (Q :set SOLUTIONS)  order-insensitive solution equality
  (Q :first SOLUTION) first solution equality
  (Q :succeeds)       provable
  (Q :fails)          not provable
  (Q :signals)        signals an error
Trailing OPTIONS (e.g. :limit 2) are passed to QUERY-PROLOG."
  `(deftest ,name ()
     (let ((%rulebase ,rulebase-form))
       (declare (ignorable %rulebase))
       ,@(mapcar #'%query-spec-assertion specs))))

(defmacro assert-query (rulebase query kind &rest rest)
  "Assert one query expectation against RULEBASE.

KIND matches DEFTEST-QUERIES:
  =>        ordered solution list equality
  :set      order-insensitive solution equality
  :first    first solution equality
  :succeeds provable
  :fails    not provable
  :signals  signals an error

Trailing OPTIONS are passed to QUERY-PROLOG or QUERY-PROLOG-FIRST."
  (multiple-value-bind (expected options)
      (case kind
        ((:succeeds :fails :signals)
         (values nil rest))
        (otherwise
         (values (first rest) (rest rest))))
    (let ((run (%query-run-form rulebase query kind options)))
      (ecase kind
        (=>        `(is-equal ',expected ,run))
        (:set      `(is-same-set ',expected ,run))
        (:first    `(is-equal ',expected ,run))
        (:succeeds `(is (not (null ,run))))
        (:fails    `(is (null ,run)))
        (:signals  `(signals-error ,run))))))
