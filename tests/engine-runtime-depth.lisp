(in-package #:cl-prolog.tests)

(deftest finite-proofs-are-unbounded-by-default ()
  (let ((rb (make-rulebase))
        (chain-length 20))
    (labels ((predicate-at (index)
               (intern (format nil "DEPTH-~D" index) *package*)))
      (rulebase-insert-clause! rb (make-clause (list (predicate-at 0))))
      (loop for index from 1 to chain-length
            do (rulebase-insert-clause!
                rb
                (make-clause (list (predicate-at index))
                             (list (list (predicate-at (1- index)))))))
      (is-equal '(nil) (query-prolog rb (list (predicate-at chain-length))))
      (is (handler-case
              (progn
                (query-prolog rb (list (predicate-at chain-length))
                              :max-depth (1- chain-length))
                nil)
            (prolog-depth-limit-exceeded () t))))))

(deftest call-with-depth-limit-does-not-scope-over-the-caller-continuation ()
  (let ((rb (make-rulebase)))
    (assert-query
     rb
     (and (call_with_depth_limit true 1 ?depth)
          (= ?side ok))
     => (((?depth . 1) (?side . ok))))))

(deftest nested-call-with-depth-limit-overrides-only-the-inner-scope ()
  (let ((rb (prolog
              ((ready))
              ((one-deep) (ready)))))
    (assert-query
     rb
     (call_with_depth_limit
      (call_with_depth_limit (one-deep) 2 ?inner-depth)
      1 ?outer-depth)
     => (((?inner-depth . 2) (?outer-depth . 1))))))

(deftest call-with-depth-limit-is-uncatchable-by-goal ()
  (let ((rb (prolog
              ((looping) (looping)))))
    (let* ((solutions
             (query-prolog
              rb
              '(call_with_depth_limit
                (catch (looping) ?caught true) 0 ?result)))
           (solution (first solutions)))
      ;; Unbound query variables are represented by self-bindings in solutions;
      ;; substituting through one would recurse indefinitely.
      (is (logic-var-p (cdr (assoc '?caught solution))))
      (is (eq (cl-prolog::%iso-atom "DEPTH_LIMIT_EXCEEDED")
              (logic-substitute '?result solution))))))

(deftest call-with-depth-limit-is-cut-opaque ()
  (let ((rb (make-rulebase)))
    (is-equal
     '(((?depth . cl-prolog::depth_limit_exceeded) (?side . ?side))
       ((?depth . ?depth) (?side . fallback)))
     (query-prolog
      rb '(or (call_with_depth_limit (and ! fail) 0 ?depth)
              (= ?side fallback))))))

(deftest call-with-depth-limit-counts-rules-and-preserves-global-limit ()
  (let ((rb (prolog
              ((ready))
              ((one-deep) (ready))
              ((two-deep) (one-deep)))))
    (assert-query rb (call_with_depth_limit true 1 ?depth)
                  => (((?depth . 1))))
    (let* ((solutions
             (query-prolog rb '(call_with_depth_limit true 0 ?result)))
           (result (logic-substitute '?result (first solutions))))
      (is (eq (cl-prolog::%iso-atom "DEPTH_LIMIT_EXCEEDED") result)))
    (assert-query rb (call_with_depth_limit (ready) 1 ?depth)
                  => (((?depth . 1))))
    (assert-query rb (call_with_depth_limit (one-deep) 2 ?depth)
                  => (((?depth . 2))))
    (assert-query rb (call_with_depth_limit (two-deep) 3 ?depth)
                  => (((?depth . 3))))
    (let* ((solutions
             (query-prolog rb '(call_with_depth_limit (ready) 0 ?result)))
           (result (logic-substitute '?result (first solutions))))
      (is (eq (cl-prolog::%iso-atom "DEPTH_LIMIT_EXCEEDED") result)))
    (let* ((solutions
             (query-prolog rb '(call_with_depth_limit (two-deep) 2 ?result)))
           (result (logic-substitute '?result (first solutions))))
      (is (eq (cl-prolog::%iso-atom "DEPTH_LIMIT_EXCEEDED") result)))
    (signals-condition prolog-depth-limit-exceeded
      (query-prolog rb '(call_with_depth_limit (one-deep) 5 ?result)
                    :max-depth 0))))

(deftest depth-counts-only-user-rule-resolution ()
  (let ((rb (prolog
              ((ready))
              ((through-call) (call ready))
              ((through-not) (not false)))))
    (is-equal '(nil) (query-prolog rb '(ready) :max-depth 0))
    (is-equal '(nil) (query-prolog rb '(through-call) :max-depth 1))
    (is-equal '(nil) (query-prolog rb '(through-not) :max-depth 1))
    (handler-case
        (progn
          (query-prolog rb '(through-call) :max-depth 0)
          (error "Expected a PROLOG-DEPTH-LIMIT-EXCEEDED"))
      (prolog-depth-limit-exceeded (condition)
        (is-equal '(through-call)
                  (prolog-depth-limit-exceeded-goal condition))))))
