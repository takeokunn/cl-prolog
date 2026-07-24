;;;; Control-flow builtin tests: call/N, call_nth, call_with_depth_limit,
;;;; setup_call_cleanup, and catch/throw interaction with continuations.
;;;; Collection/database tests live in builtin-collections.lisp; arithmetic
;;;; and flag tests live in builtin-arithmetic-and-flags.lisp.

(in-package #:cl-prolog.tests)

(defun solution-count (rulebase goal)
  "Return the number of solutions GOAL has against RULEBASE."
  (cl:length (query-prolog rulebase goal)))

(deftest canonical-iso-predicate-names-parse-and-dispatch ()
  (let ((rulebase (make-rulebase)))
    (dolist (source
             '("atom_length(hello, 5)"
               "atom_concat(hello, world, helloworld)"
               "sub_atom(abc, 1, 1, 1, b)"
               "atom_chars(abc, [a, b, c])"
               "atom_codes(abc, [65, 66, 67])"
               "char_code(a, 65)"
               "number_chars(42, ['4', '2'])"
               "number_codes(42, [52, 50])"
               "current_predicate(atom_length/2)"
               "setup_call_cleanup(true, true, true)"
               "call_cleanup(true, true)"))
      (is (prolog-succeeds-p rulebase (read-prolog-term source))))))

(deftest-queries family-relations ((make-family-rulebase))
  ((ancestor tom ?who)           :ordered (((?who . bob)) ((?who . alice)) ((?who . eve))))
  ((ancestor ?x eve)             :set (((?x . tom)) ((?x . bob)) ((?x . alice))))
  ((ancestor ?x bob)             :first ((?x . tom)))
  ((grandparent tom ?z)          :ordered (((?z . alice))))
  ((ancestor tom eve)            :succeeds)
  ((ancestor eve tom)            :fails)
  ((adult ?x)                    :set (((?x . tom)) ((?x . bob)) ((?x . alice)))))

(deftest-queries iso-not-unifiable ((make-rulebase))
  ((cl-prolog:|\\=| tom alice)   :ordered (nil))
  ((cl-prolog:|\\=| ?x ?y)      :fails)
  ((cl-prolog:|\\=| tom alice)   :succeeds))

(deftest-queries control-flow-builtins ((make-family-rulebase))
  ((true)                        :ordered (nil))
  ((cl-prolog::fail)             :fails)
  ((cl-prolog::false)            :fails)
  ((= ?x left)                   :ordered (((?x . left))))
  ((= left right)                :fails)
  ((unify_with_occurs_check ?x (node value))
                                   :ordered (((?x node value))))
  ((unify_with_occurs_check ?x (node ?x)) :fails)
  ((not (parent alice tom))      :ordered (nil))
  ((not (parent tom bob))        :fails)
  ((and (= ?goal (parent alice tom)) (not ?goal))
                                   :ordered (((?goal parent alice tom))))
  ((and (= ?goal (parent tom bob)) (not ?goal)) :fails)
  ((cl-prolog::|\\+| (parent alice tom)) :ordered (nil))
  ((cl-prolog::|\\+| (parent tom bob)) :fails)
  ((and)                         :ordered (nil))
  ((and (parent tom bob) (parent bob alice)) :ordered (nil))
  ((and (parent tom ?x) (parent ?x alice))   :ordered (((?x . bob))))
  ((or (= ?x left) (= ?x right)) :set (((?x . left)) ((?x . right))))
  ((or)                          :fails)
  ((or (parent eve ?x))          :fails)
  ((call (choice ?x))            :ordered (((?x . left)) ((?x . right))))
  ((if-then-else (choice ?x) (= ?side selected) (= ?side fallback))
                                   :ordered (((?x . left) (?side . selected))))
  ((if-then-else fail (= ?side selected) (= ?side fallback))
                                   :ordered (((?side . fallback))))
  ((soft-if-then-else (choice ?x) (= ?side selected) (= ?side fallback))
                                   :ordered (((?x . left) (?side . selected))
                                       ((?x . right) (?side . selected))))
  ((soft-if-then-else fail (= ?side selected) (= ?side fallback))
                                   :ordered (((?side . fallback))))
  ((call choice ?x)              :ordered (((?x . left)) ((?x . right))))
  ((call parent tom ?child)      :ordered (((?child . bob))))
  ((call (parent tom) ?child)    :ordered (((?child . bob))))
  ((and (= ?closure (parent tom)) (call ?closure ?child))
                                   :ordered (((?closure parent tom) (?child . bob))))
  ((call ?unbound argument)      :signals)
  ((call 42 argument)            :signals)
  ((and (= ?goal (choice ?x)) (call ?goal))
                                   :ordered (((?goal choice left) (?x . left))
                                       ((?goal choice right) (?x . right))))
  ((once (choice ?x))            :ordered (((?x . left))))
  ((once (parent nobody ?x))     :fails)
  ((call_nth (choice ?x) 2)      :ordered (((?x . right))))
  ((call_nth (choice ?x) 3)      :fails)
  ((call_nth (choice ?x) ?n)     :ordered (((?x . left) (?n . 1))
                                      ((?x . right) (?n . 2))))
  ((call_nth fail ?n)            :fails)
  ((call_nth (and (or (= ?x left) (= ?x right)) !) ?n)
                                   :ordered (((?x . left) (?n . 1))))
  ((or (call_nth (and ! fail) 1) (= ?side fallback))
                                   :ordered (((?side . fallback))))
  ((cl-prolog.user-atoms::ignore (choice ?x)) :ordered (((?x . left))))
  ((cl-prolog.user-atoms::ignore fail) :ordered (nil))
  ((forall (choice ?x) (or (= ?x left) (= ?x right))) :ordered (nil))
  ((forall (choice ?x) (= ?x left)) :fails)
  ((forall fail (throw unreachable)) :ordered (nil))
  ((and (setup_call_cleanup (assertz (cleanup-marker))
                            true
                            (retractall (cleanup-marker)))
        (not (cleanup-marker))) :ordered (nil))
  ((and (call_cleanup true (assertz (cleanup-marker)))
        (cleanup-marker))        :ordered (nil))
  ((setup_call_cleanup true true fail) :ordered (nil))
  ((setup_call_cleanup true true (throw cleanup-error)) :signals)
  ((catch (throw ball) ball (= ?x recovered))
                                   :ordered (((?x . recovered))))
  ((catch (and (= ?local discarded)
               (= ?payload carried)
               (throw (problem ?payload)))
           (problem ?value)
           (= ?result ?value))
                                   :ordered (((?local . ?local)
                                        (?payload . ?payload)
                                        (?value . carried)
                                        (?result . carried))))
  ((catch (catch (throw inner) outer fail)
           inner
           (= ?x caught-outside))
                                   :ordered (((?x . caught-outside))))
  ((catch (throw mismatch) expected true) :signals)
  ((throw ?unbound)              :signals)
  ((repeat)                      :ordered (nil nil nil) :limit 3))

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
                  :ordered
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
    (is-equal 1 (solution-count rulebase '(cleanup-finished)))
    (assert-query rulebase
                  (setup_call_cleanup fail true (throw unreachable))
                  :fails)
    (assert-query rulebase
                  (setup_call_cleanup true ! (assertz (cut-cleanup)))
                  :succeeds)
    (assert-query rulebase (cut-cleanup) :succeeds)
    (is-equal 1 (solution-count rulebase '(cut-cleanup)))
    (block first-solution
      (map-prolog-solutions
       (lambda (solution)
         (declare (cl:ignore solution))
         (return-from first-solution))
       rulebase
       '(setup_call_cleanup true (choice ?value)
                            (assertz (limited-cleanup)))))
    (assert-query rulebase (limited-cleanup) :succeeds)
    (is-equal 1 (solution-count rulebase '(limited-cleanup)))))

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
    (is-equal 1 (solution-count rulebase '(limited-cleanup)))))

(deftest nested-catch-does-not-catch-continuation-exceptions ()
  (let ((rulebase (make-rulebase)))
    (assert-query
     rulebase
     (catch (and (catch true escaped (= ?handler inner))
                 (throw escaped))
            escaped
            (= ?handler outer))
     :ordered (((?handler . outer))))))

