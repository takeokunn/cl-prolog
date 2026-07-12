;;;; Query expectation helpers.

(in-package #:cl-prolog.tests)

(defun %query-run-form (rulebase query kind options)
  "Return the engine call used to evaluate QUERY for KIND."
  (ecase kind
    ((:first)
     `(query-prolog-first ,rulebase ',query ,@options))
    ((:succeeds)
     `(query-prolog ,rulebase ',query :limit 1 ,@options))
    ((=> :set :signals :fails)
     `(query-prolog ,rulebase ',query ,@options))))

(defun %query-assertion-form (run kind expected)
  "Return the test form for RUN under KIND."
  (ecase kind
    (=>        `(is-equal ',expected ,run))
    (:set      `(is-same-set ',expected ,run))
    (:first    `(is-equal ',expected ,run))
    (:succeeds `(is (not (null ,run))))
    (:fails    `(is (null ,run)))
    (:signals  `(signals-error ,run))))

(defun %query-spec-expected-and-options (kind rest)
  "Split a query spec tail into EXPECTED and OPTIONS for KIND."
  (case kind
    ((:succeeds :fails :signals)
     (values nil rest))
    (otherwise
     (values (first rest) (rest rest)))))

(defun %query-spec-assertion (spec)
  "Compile one (QUERY KIND EXPECTED... OPTIONS...) spec into an assertion form.

The predicate kinds (:succeeds, :fails, :signals) take no EXPECTED value, so any
trailing tokens are OPTIONS (e.g. :max-depth 16).  This mirrors ASSERT-QUERY."
  (destructuring-bind (query kind &rest rest) spec
    (multiple-value-bind (expected options)
        (%query-spec-expected-and-options kind rest)
      (let ((run (%query-run-form '%rulebase query kind options)))
        (%query-assertion-form run kind expected)))))

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
  `(cl-weave:describe-sequential ,(string-downcase (symbol-name name))
     ,@(loop for spec in specs
             for index from 1
             collect `(cl-weave:it ,(format nil "case ~D: ~S" index spec)
                        (let ((%rulebase ,rulebase-form))
                          (declare (ignorable %rulebase))
                          (cl-weave:expect-has-assertions)
                          ,(%query-spec-assertion spec))))))

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
      (%query-spec-expected-and-options kind rest)
    (let ((run (%query-run-form rulebase query kind options)))
      (%query-assertion-form run kind expected))))

(defmacro with-single-query-solution ((solution solutions rulebase query &rest options)
                                      &body body)
  "Execute QUERY once, assert that it yields exactly one solution, and bind it.

SOLUTIONS receives the full result list and SOLUTION receives the first solution.
Trailing OPTIONS are passed to QUERY-PROLOG."
  `(let ((,solutions (query-prolog ,rulebase (list ,query) ,@options)))
     (is (= 1 (length ,solutions))
         "query must yield exactly one solution")
     (let ((,solution (first ,solutions)))
       ,@body)))
