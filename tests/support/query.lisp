;;;; Query expectation helpers.
;;;;
;;;; DEFTEST-QUERIES and ASSERT-QUERY are cl-prolog.tests's own query
;;;; assertions no longer here: the suite dogfoods the public
;;;; CL-PROLOG/WEAVE package (src/weave.lisp) directly, inherited via
;;;; cl-prolog.tests's :USE clause.

(in-package #:cl-prolog.tests)

(defmacro with-single-query-solution ((solution solutions rulebase query &rest options)
                                      &body body)
  "Execute QUERY once, assert that it yields exactly one solution, and bind it.

SOLUTIONS receives the full result list and SOLUTION receives the first solution.
Trailing OPTIONS are passed to QUERY-PROLOG."
  `(let ((,solutions (query-prolog ,rulebase ,query ,@options)))
     (is (= 1 (length ,solutions))
         "query must yield exactly one solution")
     (let ((,solution (first ,solutions)))
       ,@body)))
