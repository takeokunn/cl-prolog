;;;; Tests for the public cl-prolog/cl-weave helpers.

(defpackage #:cl-prolog.weave.tests
  (:use #:cl)
  (:import-from #:cl-prolog
   #:copy-rulebase
   #:invalid-max-depth-error
   #:make-clause
   #:prolog
   #:query-prolog
   #:rulebase-insert-clause!)
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
   (cl-prolog:assertz (temporary value))
   :succeeds)
  ("the next case does not observe that mutation"
   (cl-prolog:current_predicate (/ temporary 1))
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

(cl-weave:describe-sequential "copy-rulebase"
  (cl-weave:it "isolates dynamic facts from the original rulebase"
    (let* ((original (make-weave-rulebase))
           (copy (copy-rulebase original)))
      (rulebase-insert-clause! copy (make-clause '(parent alice dave)))
      (cl-weave:expect
       (length (query-prolog copy '(parent alice ?child)))
       :to-be 3)
      (cl-weave:expect
       (length (query-prolog original '(parent alice ?child)))
       :to-be 2)))

  (cl-weave:it "copies nested clause terms while preserving variable aliases"
    (let* ((variable '?item)
           (original
             (cl-prolog:make-rulebase
              :clauses
              (list (make-clause
                     (list 'linked variable (list 'node variable))
                     (list (list 'seen (list 'node variable)))))))
           (copy (copy-rulebase original))
           (original-clause
             (first (cl-prolog:rulebase-visible-clauses original)))
           (copied-clause
             (first (cl-prolog:rulebase-visible-clauses copy)))
           (original-head (cl-prolog:clause-head original-clause))
           (copied-head (cl-prolog:clause-head copied-clause))
           (original-body (cl-prolog:clause-body original-clause))
           (copied-body (cl-prolog:clause-body copied-clause)))
      (cl-weave:expect (eq original-clause copied-clause) :to-be-falsy)
      (cl-weave:expect
       (eq (second copied-head)
           (second (third copied-head)))
       :to-be-truthy)
      (cl-weave:expect
       (eq (second copied-head)
           (second (second (first copied-body))))
       :to-be-truthy)
      (setf (first (third copied-head)) 'copied-node
            (first (second (first original-body))) 'original-node)
      (cl-weave:expect (first (third original-head)) :to-be 'node)
      (cl-weave:expect (first (third copied-head)) :to-be 'copied-node)
      (cl-weave:expect
       (first (second (first original-body)))
       :to-be 'original-node)
      (cl-weave:expect
       (first (second (first copied-body)))
       :to-be 'node))))
