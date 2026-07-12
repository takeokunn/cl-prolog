(in-package #:cl-prolog.tests)

(deftest call-nth-validates-arguments ()
  (let ((rulebase (make-rulebase)))
    (dolist (goal '((call_nth ?goal 1)))
      (signals-condition prolog-instantiation-error
        (query-prolog rulebase goal)))
    (dolist (goal '((call_nth 42 1)
                    (call_nth true atom)
                    (call_nth true 1.5)))
      (signals-condition prolog-type-error
        (query-prolog rulebase goal)))
    (dolist (goal '((call_nth true 0)
                    (call_nth true -1)))
      (signals-condition prolog-domain-error
        (query-prolog rulebase goal)))))

(deftest call-with-depth-limit-validates-arguments ()
  (let ((rulebase (make-rulebase)))
    (signals-condition prolog-instantiation-error
      (query-prolog rulebase '(call_with_depth_limit true ?limit ?result)))
    (signals-condition prolog-instantiation-error
      (query-prolog rulebase '(call_with_depth_limit ?goal 0 ?result)))
    (dolist (goal '((call_with_depth_limit true atom ?result)
                    (call_with_depth_limit true 1.5 ?result)
                    (call_with_depth_limit 42 0 ?result)))
      (signals-condition prolog-type-error
        (query-prolog rulebase goal)))
    (signals-condition prolog-domain-error
      (query-prolog rulebase '(call_with_depth_limit true -1 ?result)))))

(deftest call-nth-ground-index-stops-inner-search ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase
                  (call_nth (or (= ?value first)
                                (and (assertz (visited-later-solution))
                                     (= ?value second)))
                            1)
                  =>
                  (((?value . first))))
    (assert-query rulebase
                  (current_predicate (/ visited-later-solution 0)) :fails)))

(deftest cleanup-runs-on-exception-and-early-query-exit ()
  (let ((rulebase (make-family-rulebase)))
    (assert-query rulebase
                  (catch (setup_call_cleanup
                          (assertz (cleanup-started))
                          (throw interrupted)
                          (assertz (cleanup-finished)))
                         interrupted
                         true)
                  :succeeds)
    (assert-query rulebase (cleanup-started) :succeeds)
    (assert-query rulebase (cleanup-finished) :succeeds)
    (is-equal 1 (cl:length (query-prolog rulebase '(cleanup-finished))))
    (assert-query rulebase
                  (setup_call_cleanup fail true (throw unreachable))
                  :fails)
    (assert-query rulebase
                  (setup_call_cleanup true ! (assertz (cut-cleanup)))
                  :succeeds)
    (assert-query rulebase (cut-cleanup) :succeeds)
    (is-equal 1 (cl:length (query-prolog rulebase '(cut-cleanup))))
    (block first-solution
      (map-prolog-solutions
       (lambda (solution)
         (declare (cl:ignore solution))
         (return-from first-solution))
       rulebase
       '(setup_call_cleanup true (choice ?value)
                            (assertz (limited-cleanup)))))
    (assert-query rulebase (limited-cleanup) :succeeds)
    (is-equal 1 (cl:length (query-prolog rulebase '(limited-cleanup))))))

(deftest cleanup-observes-goal-bindings-and-runs-once ()
  (let ((rulebase (make-family-rulebase)))
    (assert-query rulebase
                  (setup_call_cleanup
                   true
                   (= ?value bound)
                   (assertz (cleanup-observed ?value)))
                  :succeeds)
    (is-equal 'bound
              (solution-binding '?value
                                (query-prolog-first
                                 rulebase '(cleanup-observed ?value))))
    (is-equal 1
              (cl:length
               (query-prolog rulebase '(cleanup-observed ?value))))))

(deftest cleanup-runs-when-goal-is-not-callable ()
  (let ((rulebase (make-rulebase)))
    (signals-condition prolog-type-error
      (query-prolog
       rulebase
       '(setup_call_cleanup true 42 (assertz invalid-goal-cleanup))))
    (assert-query rulebase (invalid-goal-cleanup) :succeeds)))

(deftest cleanup-runs-after-streamed-solution-delivery ()
  (let ((rulebase (make-family-rulebase))
        (observed-values '()))
    (assert-query rulebase (assertz (enumeration-cleanup marker)) :succeeds)
    (assert-query rulebase (retractall (enumeration-cleanup marker)) :succeeds)
    (map-prolog-solutions
     (lambda (solution)
       (push (solution-binding '?value solution) observed-values)
       (assert-query rulebase (enumeration-cleanup right) :fails))
     rulebase
     '(call_cleanup (choice ?value)
                    (assertz (enumeration-cleanup ?value))))
    (is-equal '(left right) (nreverse observed-values))
    (assert-query rulebase (enumeration-cleanup right) :succeeds)
    (is-equal 1
              (cl:length
               (query-prolog rulebase '(enumeration-cleanup ?value))))))

(deftest cleanup-failure-is-ignored-and-exceptions-propagate ()
  (let ((rulebase (make-family-rulebase))
        (failure-notifications 0)
        (exception-notifications 0)
        (limited-values '()))
    (assert-query rulebase (assertz limited-cleanup) :succeeds)
    (assert-query rulebase (retractall limited-cleanup) :succeeds)
    (map-prolog-solutions
     (lambda (solution)
       (declare (cl:ignore solution))
       (incf failure-notifications))
     rulebase
     '(call_cleanup (choice ?value) fail))
    (is-equal 2 failure-notifications)
    (signals-condition prolog-exception
      (map-prolog-solutions
       (lambda (solution)
         (declare (cl:ignore solution))
         (incf exception-notifications))
       rulebase
       '(call_cleanup (choice ?value) (throw cleanup-failed))))
    (is-equal 2 exception-notifications)
    (map-prolog-solutions
     (lambda (solution)
       (push (solution-binding '?value solution) limited-values)
       (assert-query rulebase (limited-cleanup) :fails))
     rulebase
     '(call_cleanup (choice ?value) (assertz (limited-cleanup)))
     :limit 1)
    (is-equal '(left) (nreverse limited-values))
    (is-equal 1
              (cl:length (query-prolog rulebase '(limited-cleanup))))))

(deftest nested-catch-does-not-catch-continuation-exceptions ()
  (let ((rulebase (make-rulebase)))
    (assert-query
     rulebase
     (catch (and (catch true escaped (= ?handler inner))
                 (throw escaped))
            escaped
            (= ?handler outer))
     => (((?handler . outer))))))
