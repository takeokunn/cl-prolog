;;;; Test harness entry point.

(defpackage #:fx.prolog.tests
  (:use #:cl #:fx.prolog)
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

(in-package #:fx.prolog.tests)

(defparameter *support-source-files*
  '("tests/support/core.lisp"
    "tests/support/query.lisp"
    "tests/support/fixtures.lisp"))

(dolist (relative-path *support-source-files*)
  (cl-prolog.bootstrap:load-source-file relative-path))
