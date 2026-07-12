(in-package #:cl-prolog.tests)

(deftest ordinary-predicates-are-not-replayed-for-tabling ()
  (let ((rulebase (prolog
                    ((run-once) (assertz marker)))))
    (assert-query rulebase (run-once) :succeeds)
    (is-equal 1 (length (query-prolog rulebase 'marker)))))

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

(deftest predicate-index-replace-reflects-transaction ()
  (let* ((rulebase (prolog ((indexed original))))
         (transaction (cl-prolog::%copy-rulebase rulebase)))
    (rulebase-insert-clause! transaction (make-clause '(indexed committed)))
    (cl-prolog::%replace-rulebase! rulebase transaction)
    (is-equal '((indexed original) (indexed committed))
              (mapcar (lambda (entry)
                        (clause-head
                         (cl-prolog::%stored-clause-clause entry)))
                      (nth-value
                       1 (cl-prolog::%rulebase-predicate-entries
                          rulebase cl-prolog::+default-prolog-module+
                          'indexed 1))))))

(deftest predicate-index-copy-is-independent ()
  (let* ((rulebase (prolog ((indexed original))))
         (copy (cl-prolog::%copy-rulebase rulebase)))
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
                          'indexed 1))))))

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

(deftest predicate-index-keeps-logical-update-history ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (assertz (indexed first)) :succeeds)
    (assert-query rulebase (assertz (indexed second)) :succeeds)
    (let ((snapshot (cl-prolog::rulebase-revision rulebase)))
      (assert-query rulebase (asserta (indexed zeroth)) :succeeds)
      (assert-query rulebase (retract (indexed first)) :succeeds)
      (is-equal '(((?x . zeroth)) ((?x . second)))
                (query-prolog rulebase '(indexed ?x)))
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

(deftest predicate-index-excludes-unrelated-clauses-and-preserves-order ()
  (let ((rulebase (prolog
                    ((noise before))
                    ((indexed first))
                    ((indexed first extra))
                    ((other between))
                    ((indexed second))
                    ((noise after)))))
    (is-equal '(((?x . first)) ((?x . second)))
              (query-prolog rulebase '(indexed ?x)))
    (multiple-value-bind (revision entries)
        (cl-prolog::%rulebase-predicate-entries
         rulebase cl-prolog::+default-prolog-module+ 'indexed 1)
      (declare (cl:ignore revision))
      (is-equal '((indexed first) (indexed second))
                (mapcar (lambda (entry)
                          (clause-head
                           (cl-prolog::%stored-clause-clause entry)))
                        entries)))))

(deftest table-sessions-do-not-outlive-a-query-or-rulebase-revision ()
  (let ((rulebase (prolog ((value old)))))
    (is-equal '(((?x . old))) (query-prolog rulebase '(value ?x)))
    (rulebase-insert-clause! rulebase (make-clause '(value new)))
    (is-equal '(((?x . old)) ((?x . new)))
              (query-prolog rulebase '(value ?x)))))

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

(deftest builtin-proof-search-inherits-table-session ()
  (let ((*observed-table-session* nil))
    (is-equal '(nil)
              (query-prolog (make-rulebase) '(test-nested-table-session)))
    (is *observed-table-session*)))

(deftest-queries variant-tabling-terminates-three-node-left-recursion
    ((prolog
      ((cycle-a ?x) (cycle-b ?x))
      ((cycle-a a))
      ((cycle-b ?x) (cycle-c ?x))
      ((cycle-c ?x) (cycle-a ?x))
      ((cycle-c c))))
  ((cycle-a ?x) => (((?x . c)) ((?x . a)))))

(deftest-queries variant-tabling-terminates-mutual-left-recursion
    ((prolog
      ((even-node ?x) (odd-node ?x))
      ((even-node zero))
      ((odd-node ?x) (even-node ?x))
      ((odd-node one))))
  ((even-node ?x) => (((?x . one)) ((?x . zero)))))

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

(deftest-queries variant-tabling-terminates-left-recursion
    ((prolog
      ((path ?x ?y) (path ?x ?z) (edge ?z ?y))
      ((path ?x ?y) (edge ?x ?y))
      ((edge a b))
      ((edge b c))
      ((edge c d))))
  ((path a ?who)
   => (((?who . b)) ((?who . c)) ((?who . d)))))

(deftest-queries depth-bound-signals-incomplete-search
    ((prolog
      ((loop-forever ?n) (loop-forever (s ?n)))))
  ((loop-forever zero) :signals :max-depth 16))

(deftest-queries facts-are-tried-before-rules
    ((prolog
      ((color red))
      ((color ?x) (= ?x derived))))
  ((color ?x) => (((?x . red)) ((?x . derived)))))

(deftest malformed-clauses-are-ignored ()
  (let ((rb (make-rulebase)))
    (rulebase-insert-clause! rb (make-clause '() '((anything))))
    (rulebase-insert-clause! rb (make-clause '(ready)))
    (is-equal '(nil) (query-prolog rb '(ready)))))

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
