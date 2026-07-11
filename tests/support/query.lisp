;;;; Query expectation helpers and temporary builtin registration.

(in-package #:fx.prolog.tests)

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

(defmacro with-test-builtin (((names-form &rest argument-list)
                              (rulebase environment depth emit)
                              &body body)
                             &body forms)
  "Register a temporary builtin for test FORMS and evaluate them.

NAMES-FORM may evaluate to a symbol or a list of alias symbols.  The
lambda list follows DEFINE-BUILTIN so tests can exercise the same arity
rules without going through EVAL inside the timeout harness."
  (let ((arguments (gensym "ARGUMENTS"))
        (names (gensym "NAMES"))
        (saved-solvers (gensym "SAVED-SOLVERS")))
    `(let* ((,names (let ((value ,names-form))
                      (if (listp value) value (list value))))
            (,saved-solvers
              (mapcar (lambda (builtin-name)
                        (multiple-value-bind (solver present-p)
                            (gethash builtin-name fx.prolog::*builtin-solvers*)
                          (list builtin-name present-p solver)))
                      ,names)))
       (unwind-protect
            (progn
              (multiple-value-bind (minimum maximum)
                  (fx.prolog::%argument-list-arity ',argument-list)
                (fx.prolog::%register-builtins
                 ,names
                 minimum
                 maximum
                 (lambda (,arguments ,rulebase ,environment ,depth ,emit)
                   (declare (ignorable ,rulebase ,environment ,depth ,emit))
                   (destructuring-bind ,argument-list ,arguments
                     ,@body))))
              ,@forms)
         (dolist (entry ,saved-solvers)
           (destructuring-bind (builtin-name present-p previous) entry
             (if present-p
                 (setf (gethash builtin-name fx.prolog::*builtin-solvers*) previous)
                 (remhash builtin-name fx.prolog::*builtin-solvers*))))))))
