;;;; Tests for the public cl-prolog/cl-weave helpers.

(defpackage #:cl-prolog.weave.tests
  (:use #:cl #:cl-prolog)
  (:import-from #:cl-prolog/weave
   #:assert-query
   #:deftest-queries))

(in-package #:cl-prolog.weave.tests)

(defun make-weave-rulebase ()
  (prolog
    ((parent alice bob))
    ((parent alice carol))))

(deftest-queries public-query-assertions ((make-weave-rulebase))
  ("preserves solution order" (parent alice ?child) :ordered
   (((?child . bob)) ((?child . carol))))
  ((parent alice ?child) :set
   (((?child . carol)) ((?child . bob))))
  ("returns the first solution" (parent alice ?child) :first
   ((?child . bob)))
  ((parent alice bob) :succeeds)
  ((parent bob alice) :fails)
  ((parent alice bob) :signals invalid-max-depth-error
   :max-depth :invalid))

(deftest-queries fresh-query-rulebases ((prolog ((seed value))))
  ("a case may mutate its rulebase"
   (assertz (temporary value))
   :succeeds)
  ("the next case does not observe that mutation"
   (temporary value)
   :fails))

(cl-weave:describe-sequential "assert-query"
  (cl-weave:it "compares only the outer solution order"
    (cl-weave:expect-has-assertions)
    (let ((rulebase (make-weave-rulebase)))
      (assert-query rulebase (parent alice ?child) :set
        (((?child . carol)) ((?child . bob))))))

  (cl-weave:it "preserves order inside each solution"
    (cl-weave:expect
     (cl-prolog/weave::%solution-multiset-equal-p
      '(((?x . one) (?y . two)))
      '(((?y . two) (?x . one))))
     :to-be-falsy)))
