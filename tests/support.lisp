;;;; Test harness entry point.

(defpackage #:cl-prolog.tests
  (:use #:cl #:cl-prolog)
  (:shadowing-import-from #:cl-prolog #:catch #:throw)
  (:export #:run-tests
           #:deftest
           #:deftest-table
           #:deftest-queries
           #:assert-query
           #:is
           #:is-equal
           #:is-same-set
           #:signals-error
           #:make-family-rulebase))

(in-package #:cl-prolog.tests)

(defparameter *support-source-files*
  '("tests/support/core.lisp"
    "tests/support/query.lisp"
    "tests/support/fixtures.lisp"))

(dolist (relative-path *support-source-files*)
  (cl-prolog.bootstrap:load-source-file relative-path))
