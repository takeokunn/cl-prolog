;;;; Predicate-index maintenance and call/rule-resolution depth-limit tests.

(in-package #:cl-prolog.tests)

(defun stored-clause-heads (entries)
  "Return the clause head of each stored-clause entry in ENTRIES, in order."
  (mapcar (lambda (entry)
            (clause-head (cl-prolog::%stored-clause-clause entry)))
          entries))

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
              (stored-clause-heads (cl-prolog::rulebase-entries rulebase)))
    (multiple-value-bind (revision entries)
        (cl-prolog::%rulebase-predicate-entries
         rulebase cl-prolog::+default-prolog-module+ 'indexed 1)
      (declare (cl:ignore revision))
      (is-equal '((indexed zeroth) (indexed first) (indexed second))
                (stored-clause-heads entries)))
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
                  (stored-clause-heads entries))
        (is (eq (last entries)
                (gethash key
                         (cl-prolog::rulebase-predicate-tails rulebase)))))
      (is-equal '((indexed first) (indexed second))
                (stored-clause-heads
                 (cl-prolog::%rulebase-predicate-entries-at-revision
                  rulebase cl-prolog::+default-prolog-module+
                  'indexed 1 snapshot)))
      (assert-query rulebase (abolish (/ indexed 1)) :succeeds)
      (is-equal '()
                (cl-prolog::%rulebase-predicate-entries-at-revision
                 rulebase cl-prolog::+default-prolog-module+ 'indexed 1
                 (cl-prolog::rulebase-revision rulebase)))
      (is-equal '((indexed first) (indexed second))
                (stored-clause-heads
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
              (stored-clause-heads
               (cl-prolog::%rulebase-predicate-entries-at-revision
                rulebase 'alpha 'indexed 1
                (cl-prolog::rulebase-revision rulebase))))
    (is-equal '((indexed beta))
              (stored-clause-heads
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
              (stored-clause-heads
               (nth-value
                1 (cl-prolog::%rulebase-predicate-entries
                   rulebase cl-prolog::+default-prolog-module+
                   'indexed 1))))
    (is-equal '((indexed original) (indexed copied))
              (stored-clause-heads
               (nth-value
                1 (cl-prolog::%rulebase-predicate-entries
                   copy cl-prolog::+default-prolog-module+
                   'indexed 1))))
    (rulebase-insert-clause! rulebase
                             (make-clause '(indexed original-added)))
    (is-equal '((indexed original) (indexed original-added))
              (stored-clause-heads
               (nth-value
                1 (cl-prolog::%rulebase-predicate-entries
                   rulebase cl-prolog::+default-prolog-module+
                   'indexed 1))))
    (is-equal '((indexed original) (indexed copied))
              (stored-clause-heads
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
              (stored-clause-heads
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
              (stored-clause-heads
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
                (stored-clause-heads next-snapshot)))))

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
                  :ordered (((?depth . 1))))
    (let* ((solutions
             (query-prolog rb '(call_with_depth_limit true 0 ?result)))
           (result (logic-substitute '?result (first solutions))))
      (is (eq (cl-prolog::%iso-atom "DEPTH_LIMIT_EXCEEDED") result)))
    (assert-query rb (call_with_depth_limit (ready) 1 ?depth)
                  :ordered (((?depth . 1))))
    (assert-query rb (call_with_depth_limit (one-deep) 2 ?depth)
                  :ordered (((?depth . 2))))
    (assert-query rb (call_with_depth_limit (two-deep) 3 ?depth)
                  :ordered (((?depth . 3))))
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
     :ordered (((?inner-depth . 2) (?outer-depth . 1))))))

(deftest call-with-depth-limit-does-not-scope-over-the-caller-continuation ()
  (let ((rb (make-rulebase)))
    (assert-query
     rb
     (and (call_with_depth_limit true 1 ?depth)
          (= ?side ok))
     :ordered (((?depth . 1) (?side . ok))))))

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
