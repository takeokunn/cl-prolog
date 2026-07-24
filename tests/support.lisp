;;;; Test package for the ASDF-loaded cl-weave suite.

(defpackage #:cl-prolog.tests
  (:use #:cl #:cl-prolog #:cl-prolog/weave)
  (:shadowing-import-from #:cl-prolog #:assert #:catch #:throw)
  (:export #:deftest
           #:deftest-table
           #:deftest-io-variants
           #:deftest-queries
           #:assert-query
           #:with-single-query-solution
           #:is
           #:is-equal
           #:is-same-set
           #:signals-error
           #:make-family-rulebase))

(in-package #:cl-prolog.tests)
