;;;; Runtime, control, and internal engine behavior tests.

(in-package #:cl-prolog.tests)

(cl-prolog::define-builtin (test-twice input output)
    (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let ((value (logic-substitute input environment)))
    (when (numberp value)
      (cl-prolog::%unify-emit output (* 2 value) environment emit))))

(cl-prolog::define-builtin ((test-collect test-collect-alias) output &rest arguments)
    (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (cl-prolog::%unify-emit output (copy-list arguments) environment emit))

(cl-prolog::define-builtin (test-programmer-error)
    (rulebase environment depth emit)
  (declare (cl:ignore rulebase environment depth emit))
  (error "test-only Common Lisp programmer error"))

(defvar *observed-table-session* nil)

(cl-prolog::define-builtin (test-nested-table-session)
    (rulebase environment depth emit)
  (let ((outer cl-prolog::*current-table-session*))
    (cl-prolog::%prove-bindings/k
     '(true) rulebase environment depth
     (lambda (bindings)
       (setf *observed-table-session*
             (and outer
                  (eq outer cl-prolog::*current-table-session*)))
       (funcall emit bindings)))))

(defun capture-prolog-condition (thunk)
  (handler-case
      (progn
        (funcall thunk)
        (error "Expected a PROLOG-EXCEPTION"))
    (prolog-exception (condition)
      condition)))

(deftest-queries cut-prunes-clause-alternatives
    ((prolog
      ((choice left))
      ((choice right))
      ((pick ?x) (choice ?x) !)
      ((pick fallback) (choice left))
      ((commit-here) !)
      ((commit-here) fail)
      ((commit-value first) !)
      ((commit-value second))))
  ((pick ?x) => (((?x . left))))
  ((commit-value ?x) => (((?x . first))))
  (((choice ?x) (commit-here)) => (((?x . left)) ((?x . right))))
  (((choice ?x) !) => (((?x . left))))
  ((and (choice ?x) !) => (((?x . left))))
  (((choice ?x) (call !)) => (((?x . left)) ((?x . right))))
  (((choice ?x) (once (and (choice ?y) !)))
   => (((?x . left) (?y . left)) ((?x . right) (?y . left))))
  (((choice ?x) (if-then-else (and true !) true fail))
   => (((?x . left)) ((?x . right))))
  (((choice ?x) (if-then-else true ! fail)) => (((?x . left))))
  (((choice ?x) (if-then-else fail true !)) => (((?x . left))))
  )

(deftest malformed-clauses-are-ignored ()
  (let ((rb (make-rulebase)))
    (rulebase-insert-clause! rb (make-clause '() '((anything))))
    (rulebase-insert-clause! rb (make-clause '(ready)))
    (is-equal '(nil) (query-prolog rb '(ready)))))

(deftest-queries facts-are-tried-before-rules
    ((prolog
      ((color red))
      ((color ?x) (= ?x derived))))
  ((color ?x) => (((?x . red)) ((?x . derived)))))

;; The recursive argument keeps growing, so variant tabling cannot close a
;; fixed point and the explicit depth budget must fire.  (A plain P :- P
;; loop is answered finitely by tabling and would fail instead of signal.)
(deftest-queries depth-bound-signals-incomplete-search
    ((prolog
      ((loop-forever ?n) (loop-forever (s ?n)))))
  ((loop-forever zero) :signals :max-depth 16))

(deftest variant-tabling-terminates-left-recursion ()
  (let* ((edge-count 128)
         (rulebase
           (make-rulebase
            :clauses
            (append
             (list
              (make-clause
               (quote (path ?x ?y))
               (quote ((path ?x ?z) (edge ?z ?y))))
              (make-clause
               (quote (path ?x ?y))
               (quote ((edge ?x ?y)))))
             (loop for source below edge-count
                   collect (make-clause
                            (list (quote edge) source (1+ source)))))))
         (solutions (query-prolog rulebase (quote (path 0 ?who)))))
    (is-equal
     (loop for target from 1 to edge-count
           collect (list (cons (quote ?who) target)))
     solutions)))

(deftest-queries variant-tabling-deduplicates-answers
    ((prolog
      ((reachable ?x ?y) (reachable ?x ?z) (arc ?z ?y))
      ((reachable ?x ?y) (arc ?x ?y))
      ((arc a b))
      ((arc a b))
      ((arc b c))
      ((arc a c))))
  ((reachable a ?who)
   => (((?who . b)) ((?who . c)))))

(deftest-queries variant-tabling-terminates-mutual-left-recursion
    ((prolog
      ((even-node ?x) (odd-node ?x))
      ((even-node zero))
      ((odd-node ?x) (even-node ?x))
      ((odd-node one))))
  ((even-node ?x) => (((?x . one)) ((?x . zero)))))

(deftest-queries variant-tabling-terminates-three-node-left-recursion
    ((prolog
      ((cycle-a ?x) (cycle-b ?x))
      ((cycle-a a))
      ((cycle-b ?x) (cycle-c ?x))
      ((cycle-c ?x) (cycle-a ?x))
      ((cycle-c c))))
  ((cycle-a ?x) => (((?x . c)) ((?x . a)))))

(deftest tabled-predicate-preserves-and-deduplicates-cyclic-answer (:timeout 2)
  (let* ((cycle-a (cons 'loop nil))
         (cycle-b (cons 'loop nil))
         (rulebase (make-rulebase)))
    (setf (cdr cycle-a) cycle-a
          (cdr cycle-b) cycle-b)
    (rulebase-insert-clause!
     rulebase (make-clause (list 'cyclic-answer cycle-a)))
    (rulebase-insert-clause!
     rulebase (make-clause (list 'cyclic-answer cycle-b)))
    (cl-prolog::%add-rulebase-table-declaration!
     rulebase 'cyclic-answer 1 :test)
    (let* ((solutions (query-prolog rulebase '(cyclic-answer ?answer)))
           (answer (solution-binding '?answer (first solutions))))
      (is-equal 1 (length solutions))
      (is (consp answer))
      (is (eq answer (cdr answer)))
      (is-equal 'loop (car answer)))))

(deftest left-recursion-through-leading-builtins-and-control-terminates
    (:timeout 2)
  (let ((rulebase
          (prolog
           ((builtin-direct ?x) true (builtin-direct ?x))
           ((builtin-direct direct-base))
           ((builtin-indirect-p ?x) (= ?x ?y) (builtin-indirect-q ?y))
           ((builtin-indirect-p p-base))
           ((builtin-indirect-q ?x) true (builtin-indirect-p ?x))
           ((builtin-indirect-q q-base))
           ((control-direct ?x)
            (call (and true (control-direct ?x))))
           ((control-direct control-base)))))
    (is-equal '(((?x . direct-base)))
              (query-prolog rulebase '(builtin-direct ?x)))
    (is-same-set '(((?x . p-base)) ((?x . q-base)))
                 (query-prolog rulebase '(builtin-indirect-p ?x)))
    (is-equal '(((?x . control-base)))
              (query-prolog rulebase '(control-direct ?x)))))

(deftest builtin-proof-search-inherits-table-session ()
  (let ((*observed-table-session* nil))
    (is-equal '(nil)
              (query-prolog (make-rulebase) '(test-nested-table-session)))
    (is *observed-table-session*)))

(deftest interrupted-table-build-discards-partial-entry ()
  (let* ((rulebase (prolog
                     ((recursive ?x) (recursive ?x))
                     ((recursive value))))
         (session (cl-prolog::%make-rulebase-table-session rulebase))
         (state (cl-prolog::%make-proof-state
                 rulebase '() nil cl-prolog::+default-prolog-module+ session
                 (cl-prolog::%make-cut-tag))))
    (handler-case
        (cl-prolog::%prove-clauses/k
         '(recursive ?x) state
         (lambda (answer-state)
           (declare (cl:ignore answer-state))
           (error "interrupt table construction")))
      (error () nil))
    (is-equal 0
              (hash-table-count
               (cl-prolog::%table-session-entries session)))))

(deftest table-sessions-do-not-outlive-a-query-or-rulebase-revision ()
  (let ((rulebase (prolog ((value old)))))
    (is-equal '(((?x . old))) (query-prolog rulebase '(value ?x)))
    (rulebase-insert-clause! rulebase (make-clause '(value new)))
    (is-equal '(((?x . old)) ((?x . new)))
              (query-prolog rulebase '(value ?x)))))

(deftest predicate-index-excludes-unrelated-clauses-and-preserves-order ()
  (let* ((rulebase (make-rulebase))
         (index (cl-prolog::rulebase-predicate-index rulebase))
         (tails (cl-prolog::rulebase-predicate-tails rulebase))
         (key (list cl-prolog::+default-prolog-module+ 'indexed 1)))
    (is (null (cl-prolog::rulebase-entries rulebase)))
    (is (null (cl-prolog::rulebase-entries-tail rulebase)))
    (is-equal 0 (hash-table-count index))
    (is-equal 0 (hash-table-count tails))
    (rulebase-insert-clause! rulebase (make-clause '(indexed first))
                             :position :first)
    (is (eq (cl-prolog::rulebase-entries rulebase)
            (cl-prolog::rulebase-entries-tail rulebase)))
    (is (eq (gethash key index) (gethash key tails)))
    (let ((first-indexed-tail (gethash key tails)))
      (rulebase-insert-clause! rulebase (make-clause '(other between)))
      (is (eq first-indexed-tail (gethash key tails))))
    (rulebase-insert-clause! rulebase (make-clause '(indexed second)))
    (let ((global-tail (cl-prolog::rulebase-entries-tail rulebase))
          (indexed-tail (gethash key tails)))
      (rulebase-insert-clause! rulebase (make-clause '(indexed zeroth))
                               :position :first)
      (is (eq global-tail (cl-prolog::rulebase-entries-tail rulebase)))
      (is (eq indexed-tail (gethash key tails))))
    (is-equal '((indexed zeroth)
                (indexed first)
                (other between)
                (indexed second))
              (mapcar (lambda (entry)
                        (clause-head
                         (cl-prolog::%stored-clause-clause entry)))
                      (cl-prolog::rulebase-entries rulebase)))
    (multiple-value-bind (revision entries)
        (cl-prolog::%rulebase-predicate-entries
         rulebase cl-prolog::+default-prolog-module+ 'indexed 1)
      (declare (cl:ignore revision))
      (is-equal '((indexed zeroth) (indexed first) (indexed second))
                (mapcar (lambda (entry)
                          (clause-head
                           (cl-prolog::%stored-clause-clause entry)))
                        entries)))
    (is (eq (last (cl-prolog::rulebase-entries rulebase))
            (cl-prolog::rulebase-entries-tail rulebase)))
    (is (loop for predicate-key being the hash-keys of index
                using (hash-value entries)
              always
              (and (eq (last entries) (gethash predicate-key tails))
                   (equal entries
                          (remove-if-not
                           (lambda (entry)
                             (equal predicate-key
                                    (cl-prolog::%stored-clause-predicate-key
                                     entry)))
                           (cl-prolog::rulebase-entries rulebase))))))))

(deftest predicate-index-keeps-logical-update-history ()
  (let* ((rulebase (make-rulebase))
         (key (list cl-prolog::+default-prolog-module+ 'indexed 1)))
    (assert-query rulebase (assertz (indexed first)) :succeeds)
    (assert-query rulebase (assertz (indexed second)) :succeeds)
    (let ((snapshot (cl-prolog::rulebase-revision rulebase)))
      (assert-query rulebase (asserta (indexed zeroth)) :succeeds)
      (assert-query rulebase (retract (indexed first)) :succeeds)
      (is-equal '(((?x . zeroth)) ((?x . second)))
                (query-prolog rulebase '(indexed ?x)))
      (assert-query rulebase (assertz (indexed third)) :succeeds)
      (is-equal '(((?x . zeroth)) ((?x . second)) ((?x . third)))
                (query-prolog rulebase '(indexed ?x)))
      (let ((entries
              (gethash key
                       (cl-prolog::rulebase-predicate-index rulebase))))
        (is-equal '((indexed zeroth)
                    (indexed first)
                    (indexed second)
                    (indexed third))
                  (mapcar
                   (lambda (entry)
                     (clause-head
                      (cl-prolog::%stored-clause-clause entry)))
                   entries))
        (is (eq (last entries)
                (gethash key
                         (cl-prolog::rulebase-predicate-tails rulebase)))))
      (is-equal '((indexed first) (indexed second))
                (mapcar
                 (lambda (entry)
                   (clause-head (cl-prolog::%stored-clause-clause entry)))
                 (cl-prolog::%rulebase-predicate-entries-at-revision
                  rulebase cl-prolog::+default-prolog-module+
                  'indexed 1 snapshot)))
      (assert-query rulebase (abolish (/ indexed 1)) :succeeds)
      (is-equal '()
                (cl-prolog::%rulebase-predicate-entries-at-revision
                 rulebase cl-prolog::+default-prolog-module+ 'indexed 1
                 (cl-prolog::rulebase-revision rulebase)))
      (is-equal '((indexed first) (indexed second))
                (mapcar
                 (lambda (entry)
                   (clause-head (cl-prolog::%stored-clause-clause entry)))
                 (cl-prolog::%rulebase-predicate-entries-at-revision
                  rulebase cl-prolog::+default-prolog-module+
                  'indexed 1 snapshot))))))

(deftest predicate-index-isolates-modules ()
  (let ((rulebase (make-rulebase)))
    (rulebase-insert-clause! rulebase (make-clause '(indexed alpha))
                             :module 'alpha)
    (rulebase-insert-clause! rulebase (make-clause '(indexed beta))
                             :module 'beta)
    (is-equal '((indexed alpha))
              (mapcar (lambda (entry)
                        (clause-head
                         (cl-prolog::%stored-clause-clause entry)))
                      (cl-prolog::%rulebase-predicate-entries-at-revision
                       rulebase 'alpha 'indexed 1
                       (cl-prolog::rulebase-revision rulebase))))
    (is-equal '((indexed beta))
              (mapcar (lambda (entry)
                        (clause-head
                         (cl-prolog::%stored-clause-clause entry)))
                      (cl-prolog::%rulebase-predicate-entries-at-revision
                       rulebase 'beta 'indexed 1
                       (cl-prolog::rulebase-revision rulebase))))))

(deftest predicate-index-copy-is-independent ()
  (let* ((rulebase (prolog ((indexed original))))
         (copy (cl-prolog::%copy-rulebase rulebase))
         (key (list cl-prolog::+default-prolog-module+ 'indexed 1)))
    (is (not (eq (cl-prolog::rulebase-entries rulebase)
                 (cl-prolog::rulebase-entries copy))))
    (is (not (eq (cl-prolog::rulebase-entries-tail rulebase)
                 (cl-prolog::rulebase-entries-tail copy))))
    (is (not (eq (cl-prolog::rulebase-predicate-index rulebase)
                 (cl-prolog::rulebase-predicate-index copy))))
    (is (not (eq (gethash key
                          (cl-prolog::rulebase-predicate-index rulebase))
                 (gethash key
                          (cl-prolog::rulebase-predicate-index copy)))))
    (is (not (eq (gethash key
                          (cl-prolog::rulebase-predicate-tails rulebase))
                 (gethash key
                          (cl-prolog::rulebase-predicate-tails copy)))))
    (rulebase-insert-clause! copy (make-clause '(indexed copied)))
    (is-equal '((indexed original))
              (mapcar (lambda (entry)
                        (clause-head
                         (cl-prolog::%stored-clause-clause entry)))
                      (nth-value
                       1 (cl-prolog::%rulebase-predicate-entries
                          rulebase cl-prolog::+default-prolog-module+
                          'indexed 1))))
    (is-equal '((indexed original) (indexed copied))
              (mapcar (lambda (entry)
                        (clause-head
                         (cl-prolog::%stored-clause-clause entry)))
                      (nth-value
                       1 (cl-prolog::%rulebase-predicate-entries
                          copy cl-prolog::+default-prolog-module+
                          'indexed 1))))
    (rulebase-insert-clause! rulebase
                             (make-clause '(indexed original-added)))
    (is-equal '((indexed original) (indexed original-added))
              (mapcar (lambda (entry)
                        (clause-head
                         (cl-prolog::%stored-clause-clause entry)))
                      (nth-value
                       1 (cl-prolog::%rulebase-predicate-entries
                          rulebase cl-prolog::+default-prolog-module+
                          'indexed 1))))
    (is-equal '((indexed original) (indexed copied))
              (mapcar (lambda (entry)
                        (clause-head
                         (cl-prolog::%stored-clause-clause entry)))
                      (nth-value
                       1 (cl-prolog::%rulebase-predicate-entries
                          copy cl-prolog::+default-prolog-module+
                          'indexed 1))))
    (is (eq (last (cl-prolog::rulebase-entries rulebase))
            (cl-prolog::rulebase-entries-tail rulebase)))
    (is (eq (last (cl-prolog::rulebase-entries copy))
            (cl-prolog::rulebase-entries-tail copy)))))

(deftest predicate-index-replace-reflects-transaction ()
  (let* ((rulebase (prolog ((indexed original))))
         (transaction (cl-prolog::%copy-rulebase rulebase))
         (key (list cl-prolog::+default-prolog-module+ 'indexed 1)))
    (rulebase-insert-clause! transaction (make-clause '(indexed committed)))
    (cl-prolog::%replace-rulebase! rulebase transaction)
    (is-equal '((indexed original) (indexed committed))
              (mapcar (lambda (entry)
                        (clause-head
                         (cl-prolog::%stored-clause-clause entry)))
                      (nth-value
                       1 (cl-prolog::%rulebase-predicate-entries
                          rulebase cl-prolog::+default-prolog-module+
                          'indexed 1))))
    (let ((discarded (cl-prolog::%copy-rulebase rulebase)))
      (rulebase-insert-clause! discarded
                               (make-clause '(indexed rolled-back))))
    (rulebase-insert-clause! rulebase
                             (make-clause '(indexed after-rollback)))
    (is-equal '((indexed original)
                (indexed committed)
                (indexed after-rollback))
              (mapcar (lambda (entry)
                        (clause-head
                         (cl-prolog::%stored-clause-clause entry)))
                      (nth-value
                       1 (cl-prolog::%rulebase-predicate-entries
                          rulebase cl-prolog::+default-prolog-module+
                          'indexed 1))))
    (is (eq (last (cl-prolog::rulebase-entries rulebase))
            (cl-prolog::rulebase-entries-tail rulebase)))
    (is (eq (last (gethash key
                           (cl-prolog::rulebase-predicate-index rulebase)))
            (gethash key
                     (cl-prolog::rulebase-predicate-tails rulebase))))))

(deftest predicate-index-proof-cache-follows-rulebase-revisions ()
  (let* ((rulebase (prolog ((indexed original))))
         (session (cl-prolog::%make-rulebase-table-session rulebase))
         (state (cl-prolog::%make-proof-state
                 rulebase '() nil cl-prolog::+default-prolog-module+ session
                 (cl-prolog::%make-cut-tag)))
         (first-snapshot
           (cl-prolog::%proof-predicate-entries '(indexed ?value) state)))
    (is (eq first-snapshot
            (cl-prolog::%proof-predicate-entries '(indexed ?value) state)))
    (rulebase-insert-clause! rulebase (make-clause '(indexed added)))
    (let ((next-snapshot
            (cl-prolog::%proof-predicate-entries '(indexed ?value) state)))
      (is (not (eq first-snapshot next-snapshot)))
      (is-equal '((indexed original) (indexed added))
                (mapcar (lambda (entry)
                          (clause-head
                           (cl-prolog::%stored-clause-clause entry)))
                        next-snapshot)))))

(deftest ordinary-predicates-are-not-replayed-for-tabling ()
  (let ((rulebase (prolog
                    ((run-once) (assertz marker)))))
    (assert-query rulebase (run-once) :succeeds)
    (is-equal 1 (length (query-prolog rulebase 'marker)))))

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

(deftest call-with-depth-limit-is-cut-opaque ()
  (let ((rb (make-rulebase)))
    (is-equal
     '(((?depth . cl-prolog::depth_limit_exceeded) (?side . ?side))
       ((?depth . ?depth) (?side . fallback)))
     (query-prolog
      rb '(or (call_with_depth_limit (and ! fail) 0 ?depth)
              (= ?side fallback))))))

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

(deftest call-with-depth-limit-does-not-scope-over-the-caller-continuation ()
  (let ((rb (make-rulebase)))
    (assert-query
     rb
     (and (call_with_depth_limit true 1 ?depth)
          (= ?side ok))
     => (((?depth . 1) (?side . ok))))))

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

(deftest foreign-predicate-cps-solutions ()
  (let ((rb (make-family-rulebase))
        (*recorded-colors* '()))
    (assert-query rb (accepted :ok) => (nil))
    (assert-query rb (accepted :ng) => ())
    (assert-query rb (colored ?x) => (((?x . tom))))
    (is-equal '(tom) *recorded-colors*)
    (assert-query rb (foreign-choice ?value)
                  => (((?value . left)) ((?value . right))))))

(deftest foreign-predicate-dispatches-by-exact-indicator ()
  (let ((rb (prolog
              ((foreign-choice fallback zero))
              ((foreign-choice fallback)))))
    ;; No FOREIGN-CHOICE/0 solver or clause exists, so ISO requires an
    ;; existence error rather than a silent failure.
    (assert-query rb (foreign-choice) :signals)
    (assert-query rb (foreign-choice fallback zero) => (nil))
    (assert-query rb (foreign-choice fallback) => ())))

(deftest registered-foreign-predicate-is-authoritative ()
  (let ((rb (prolog ((foreign-choice clause-solution)))))
    (assert-query rb (foreign-choice ?value)
                  => (((?value . left)) ((?value . right))))))

(deftest define-foreign-predicate-registers-name-and-arity ()
  (with-macroexpansion (expansion
                        '(define-foreign-predicate (foreign-example value)
                             (rulebase environment depth emit)
                           (declare (cl:ignore rulebase depth))
                           (funcall emit environment)))
    (is (%tree-contains-p expansion 'defmethod))
    (is (%tree-contains-p expansion 'cl-prolog::%foreign-goal-solver))
    (is (%tree-contains-p expansion 'foreign-example))
    (is (%tree-contains-p expansion 1))))

(deftest-table runtime-when-guards-take-functions ()
  (:equal '(((?x . bob)))
          (query-prolog (make-family-rulebase)
                        (list 'and
                              '(parent tom ?x)
                              (list :when (lambda (x) (eq x 'bob)) '?x))))
  (:signals (query-prolog (make-family-rulebase) '(:when (equal 'bob 'bob)))))

(deftest query-limits-and-streaming ()
  (let ((rb (make-family-rulebase)))
    (assert-query rb (ancestor tom ?who) => (((?who . bob))) :limit 1)
    (is-equal 2 (length (query-prolog rb '(ancestor tom ?who) :limit 2)))
    (is (signals-error (query-prolog rb '(ancestor tom ?who) :limit 0)))
    (let ((seen '()))
      (map-prolog-solutions (lambda (solution) (push solution seen))
                            rb '(ancestor tom ?who) :limit 2)
      (is-equal '(((?who . bob)) ((?who . alice))) (reverse seen)))
    (let ((raw (query-prolog rb '(ancestor ?x bob) :project nil)))
      (is-equal 1 (length raw))
      (is (assoc '?x (first raw))))
    (handler-case
        (progn
          (query-prolog rb '(ancestor tom ?who) :max-depth -1)
          (error "Expected an INVALID-MAX-DEPTH-ERROR"))
      (invalid-max-depth-error (condition)
        (is-equal -1 (invalid-max-depth-error-value condition))))
    (is (signals-error
         (prolog-succeeds-p rb '(ancestor tom ?who) :max-depth 1.5)))))

(deftest ground-recursive-query-has-one-projected-solution ()
  (is-equal '(nil)
            (query-prolog (make-family-rulebase) '(ancestor tom eve))))

(deftest-table default-query-projection-paths ()
  (:equal '(((?x . tom)))
          (let ((seen '()))
            (map-prolog-solutions (lambda (solution) (push solution seen))
                                  (make-family-rulebase)
                                  '(ancestor ?x bob))
            (nreverse seen)))
  (:equal '(((?x . tom)))
          (query-prolog (make-family-rulebase) '(ancestor ?x bob))))

(deftest rule-variables-are-freshened-per-use ()
  (let ((rb (prolog
              ((same ?x ?x))
              ((both ?a ?b) (same ?a ?a) (same ?b ?b)))))
    (assert-query rb ((same ?p 1) (same ?q 2) (both ?p ?q))
                  => (((?p . 1) (?q . 2))))))

(deftest define-builtin-is-extensible ()
  (is-equal '(((?y . 6)))
            (query-prolog (make-rulebase) '(test-twice 3 ?y))))

(deftest builtin-does-not-shadow-user-predicate-with-different-arity ()
  (let ((rulebase (prolog ((test-twice user-defined)))))
    (is-equal '(())
              (query-prolog rulebase '(test-twice user-defined)))))


(deftest define-builtin-supports-aliases-and-rest-arguments ()
  (is-equal '(((?arguments . (a b c))))
            (query-prolog (make-rulebase)
                          '(test-collect-alias ?arguments a b c))))

(deftest define-builtin-macroexpand-registers-single-name ()
  (with-macroexpansion (expansion
                        '(cl-prolog::define-builtin (twice input output)
                           (rulebase environment depth emit)
                           (declare (cl:ignore rulebase depth))
                           (cl-prolog::%unify-emit output
                                                   (* 2 (logic-substitute input environment))
                                                   environment
                                                   emit)))
    (is (%tree-contains-p expansion 'cl-prolog::%register-builtin-solver!))
    (is (%tree-contains-p expansion 'eval-when))
    (is (%tree-contains-p expansion 'twice))))

(deftest define-builtin-macroexpand-registers-aliases-and-rest ()
  (with-macroexpansion (expansion
                        '(cl-prolog::define-builtin ((collect collect-alias) output &rest arguments)
                           (rulebase environment depth emit)
                           (declare (cl:ignore rulebase depth))
                           (cl-prolog::%unify-emit output
                                                   (copy-list arguments)
                                                   environment
                                                   emit)))
    (is (%tree-contains-p expansion 'cl-prolog::%register-builtin-solver!))
    (is (%tree-contains-p expansion 'eval-when))
    (is (%tree-contains-p expansion 'collect))
    (is (%tree-contains-p expansion 'collect-alias))))

(deftest-table invalid-goal-error-reports-context ()
  ;; A builtin name used at the wrong arity denotes a different, undefined
  ;; procedure (=/1), so ISO requires an existence error carrying its
  ;; predicate indicator.
  (:equal '(/ = 1)
          (handler-case
              (progn
                (query-prolog (make-rulebase) '(= a))
                (error "Expected a PROLOG-EXISTENCE-ERROR"))
            (prolog-existence-error (condition)
              (third (second (prolog-exception-term condition))))))
  (:equal '(42 x)
          (handler-case
              (progn
                (query-prolog (make-rulebase) '((42 x)))
                (error "Expected an INVALID-GOAL-ERROR"))
            (invalid-goal-error (condition)
              (invalid-goal-error-goal condition))))
  (:equal '("INSTANTIATION_ERROR" "CALL")
          (handler-case
              (progn
                (query-prolog (make-rulebase) '?goal)
                (error "Expected a PROLOG-INSTANTIATION-ERROR"))
            (prolog-instantiation-error (condition)
              (let ((term (prolog-exception-term condition)))
                (list (symbol-name (second term))
                      (symbol-name (second (third term))))))))
  (:equal '(((?goal . true)) t)
          (multiple-value-list
           (query-prolog-first
            (make-rulebase) '?goal :environment '((?goal . true)))))
  (:equal '(((?goal . true)))
          (let ((solutions '()))
            (map-prolog-solutions
             (lambda (solution) (push solution solutions))
             (make-rulebase) '?goal :environment '((?goal . true)))
            (nreverse solutions)))
  (:equal '(((?goal ready ok)) t)
          (multiple-value-list
           (query-prolog-first
            (make-rulebase :clauses (list (make-clause '(ready ok))))
            '?goal :environment '((?goal . (ready ok))))))
  (:equal '(("TYPE_ERROR" "CALLABLE" 42) "CALL" 42)
          (handler-case
              (progn
                (query-prolog
                 (make-rulebase) '?goal :environment '((?goal . 42)))
                (error "Expected an INVALID-GOAL-ERROR"))
            (invalid-goal-error (condition)
              (let ((term (prolog-exception-term condition)))
                (list (normalize-error-data (second term))
                      (symbol-name (second (third term)))
                      (invalid-goal-error-goal condition))))))
  (:equal '(p . tail)
          (handler-case
              (progn
                (query-prolog (make-rulebase) '(p . tail))
                (error "Expected an INVALID-GOAL-ERROR"))
            (invalid-goal-error (condition)
              (invalid-goal-error-goal condition))))
  (:is
   (let ((goal (list (cl-prolog::%prolog-symbol ":")
                     'missing-module
                     (cons 'p 'tail))))
     (handler-case
         (progn
           (query-prolog (make-rulebase) goal)
           nil)
       (invalid-goal-error (condition)
         (equal goal (invalid-goal-error-goal condition))))))
  (:equal '("INSTANTIATION_ERROR" "CALL")
          (let ((goal (list (cl-prolog::%prolog-symbol ":")
                            'missing-module
                            '?goal)))
            (handler-case
                (progn
                  (query-prolog (make-rulebase) goal)
                  (error "Expected a PROLOG-INSTANTIATION-ERROR"))
              (prolog-instantiation-error (condition)
                (let ((term (prolog-exception-term condition)))
                  (list (symbol-name (second term))
                        (symbol-name (second (third term))))))))))

(deftest iso-error-constructors-preserve-formal-data ()
  (dolist (case
           (list
            (list 'prolog-instantiation-error
                  (lambda ()
                    (cl-prolog::%raise-instantiation-error
                     nil (cl-prolog::%iso-atom "TEST") "test"))
                  "INSTANTIATION_ERROR")
            (list 'prolog-type-error
                  (lambda ()
                    (cl-prolog::%raise-type-error
                     "INTEGER" 'bad nil (cl-prolog::%iso-atom "TEST") "test"))
                  "TYPE_ERROR")
            (list 'prolog-domain-error
                  (lambda ()
                    (cl-prolog::%raise-domain-error
                     "NOT_LESS_THAN_ZERO" -1 nil
                     (cl-prolog::%iso-atom "TEST") "test"))
                  "DOMAIN_ERROR")
            (list 'prolog-permission-error
                  (lambda ()
                    (cl-prolog::%raise-permission-error
                     "MODIFY" "STATIC_PROCEDURE" 'test nil
                     (cl-prolog::%iso-atom "TEST") "test"))
                  "PERMISSION_ERROR")
            (list 'prolog-existence-error
                  (lambda ()
                    (cl-prolog::%raise-existence-error
                     "PROCEDURE" 'missing nil
                     (cl-prolog::%iso-atom "TEST") "test"))
                  "EXISTENCE_ERROR")
            (list 'prolog-evaluation-error
                  (lambda ()
                    (cl-prolog::%raise-evaluation-error
                     "ZERO_DIVISOR" nil (cl-prolog::%iso-atom "TEST") "test"))
                  "EVALUATION_ERROR")
            (list 'prolog-resource-error
                  (lambda ()
                    (cl-prolog::%raise-resource-error
                     "DEPTH_LIMIT" nil (cl-prolog::%iso-atom "TEST") "test"))
                  "RESOURCE_ERROR")))
    (destructuring-bind (condition-type thunk formal-name) case
      (let* ((condition (capture-prolog-condition thunk))
             (formal (second (prolog-exception-term condition))))
        (is (typep condition condition-type))
        (is-equal formal-name
                  (symbol-name (if (consp formal) (first formal) formal)))))))

(deftest catch-handles-public-prolog-runtime-errors ()
  (let ((rb (make-rulebase)))
    (dolist (query
             '("catch(X is 1 / 0, error(evaluation_error(zero_divisor), C), true)"
               "catch(X is Y, error(instantiation_error, C), true)"
               "catch(call(42), error(type_error(callable, 42), C), true)"))
      (is (prolog-succeeds-p rb (read-prolog-term query))))
    (let* ((loop-goal (list (read-prolog-term "loop")))
           (recursive-rb
             (make-rulebase
              :clauses (list (make-clause loop-goal (list loop-goal))))))
      (is (prolog-succeeds-p
           recursive-rb
           (read-prolog-term
            "catch(loop, error(resource_error(depth_limit), C), true)")
           :max-depth 0)))))

(deftest-table goal-invocation-reports-iso-errors ()
  (:is (typep (capture-prolog-condition
               (lambda () (query-prolog (make-rulebase) '(call ?goal argument))))
              'prolog-instantiation-error))
  (:is (typep (capture-prolog-condition
               (lambda () (query-prolog (make-rulebase) '(call 42 argument))))
              'prolog-type-error))
  (:is (typep (capture-prolog-condition
               (lambda () (query-prolog (make-rulebase) '(missing value))))
              'prolog-existence-error))
  (:is (typep (capture-prolog-condition
               (lambda () (query-prolog (make-rulebase) '(not (missing)))))
              'prolog-existence-error)))

(deftest unknown-procedure-error-preserves-predicate-indicator ()
  (let* ((condition
           (capture-prolog-condition
            (lambda () (query-prolog (make-rulebase) '(missing value)))))
         (formal (second (prolog-exception-term condition))))
    (is-equal "EXISTENCE_ERROR" (symbol-name (first formal)))
    (is-equal "PROCEDURE" (symbol-name (second formal)))
    ;; Predicate indicators are ordinary compound terms: functor first.
    (destructuring-bind (slash predicate arity) (third formal)
      (is-equal "/" (symbol-name slash))
      (is-equal 'missing predicate)
      (is-equal 1 arity))))

(deftest catch-does-not-handle-common-lisp-errors ()
  (is (handler-case
          (progn
            (query-prolog
             (make-rulebase)
             '(catch (test-programmer-error) ?error true))
            nil)
        (prolog-exception () nil)
        (simple-error () t))))

(deftest-table query-normalization-internals ()
  (:equal '() (cl-prolog::%normalize-query nil))
  (:equal '((parent tom bob))
          (cl-prolog::%normalize-query '(parent tom bob)))
  (:equal '((parent tom bob) (parent bob alice))
          (cl-prolog::%normalize-query '((parent tom bob) (parent bob alice))))
  (:equal '(!) (cl-prolog::%normalize-query '!))
  (:is-not (eq (cl-prolog::%make-cut-tag) (cl-prolog::%make-cut-tag))
           "Every cut barrier must carry a distinct catch tag"))

(deftest halt-requests-processor-exit ()
  (let ((rulebase (make-rulebase)))
    (flet ((halt-code (query)
             (handler-case
                 (progn (query-prolog rulebase query) :no-halt)
               (prolog-halt (condition) (prolog-halt-code condition)))))
      (is-equal 0 (halt-code '((cl-prolog::halt))))
      (is-equal 7 (halt-code '((cl-prolog::halt 7))))
      ;; catch/3 must not intercept a halt request.
      (is-equal 2 (halt-code '((cl-prolog::catch (cl-prolog::halt 2)
                                                 ?any cl-prolog:true))))
      (signals-error (query-prolog rulebase '((cl-prolog::halt ?code))))
      (signals-error (query-prolog rulebase '((cl-prolog::halt seven)))))))
