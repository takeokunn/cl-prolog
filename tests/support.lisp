;;;; Test package for the ASDF-loaded cl-weave suite.

(defpackage #:cl-prolog.tests
  (:use #:cl #:cl-prolog)
  (:shadowing-import-from #:cl-prolog #:catch #:throw)
  (:export #:deftest
           #:deftest-table
           #:deftest-queries
           #:assert-query
           #:is
           #:is-equal
           #:is-same-set
           #:signals-error
           #:make-family-rulebase))

(in-package #:cl-prolog.tests)
