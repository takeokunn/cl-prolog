;;;; Cut, tabling, and left-recursion detection tests.  Predicate-index and
;;;; depth-limit tests live in engine-runtime-index-and-depth.lisp; foreign
;;;; predicates and define-builtin registration in
;;;; engine-runtime-foreign-and-registration.lisp; the ISO error contract in
;;;; engine-runtime-error-contract.lisp.

(in-package #:cl-prolog.tests)

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
  ((pick ?x) :ordered (((?x . left))))
  ((commit-value ?x) :ordered (((?x . first))))
  (((choice ?x) (commit-here)) :ordered (((?x . left)) ((?x . right))))
  (((choice ?x) !) :ordered (((?x . left))))
  ((and (choice ?x) !) :ordered (((?x . left))))
  (((choice ?x) (call !)) :ordered (((?x . left)) ((?x . right))))
  (((choice ?x) (once (and (choice ?y) !)))
   :ordered (((?x . left) (?y . left)) ((?x . right) (?y . left))))
  (((choice ?x) (if-then-else (and true !) true fail))
   :ordered (((?x . left)) ((?x . right))))
  (((choice ?x) (if-then-else true ! fail)) :ordered (((?x . left))))
  (((choice ?x) (if-then-else fail true !)) :ordered (((?x . left))))
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
  ((color ?x) :ordered (((?x . red)) ((?x . derived)))))

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
   :ordered (((?who . b)) ((?who . c)))))

(deftest-queries variant-tabling-terminates-mutual-left-recursion
    ((prolog
      ((even-node ?x) (odd-node ?x))
      ((even-node zero))
      ((odd-node ?x) (even-node ?x))
      ((odd-node one))))
  ((even-node ?x) :ordered (((?x . one)) ((?x . zero)))))

(deftest-queries variant-tabling-terminates-three-node-left-recursion
    ((prolog
      ((cycle-a ?x) (cycle-b ?x))
      ((cycle-a a))
      ((cycle-b ?x) (cycle-c ?x))
      ((cycle-c ?x) (cycle-a ?x))
      ((cycle-c c))))
  ((cycle-a ?x) :ordered (((?x . c)) ((?x . a)))))

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

(deftest table-declaration-and-clause-retraction-guard-repeat-updates ()
  (let ((rulebase (make-rulebase)))
    (cl-prolog::%add-rulebase-table-declaration!
     rulebase 'repeat-owner 1 :owner)
    (cl-prolog::%add-rulebase-table-declaration!
     rulebase 'repeat-owner 1 :owner)
    (is (cl-prolog::%rulebase-tabled-p rulebase 'repeat-owner 1))
    (cl-prolog::%remove-rulebase-table-declaration!
     rulebase 'repeat-owner 1 :absent-owner)
    (is (cl-prolog::%rulebase-tabled-p rulebase 'repeat-owner 1))
    (cl-prolog::%remove-rulebase-table-declaration!
     rulebase 'repeat-owner 1 :owner)
    (is (not (cl-prolog::%rulebase-tabled-p rulebase 'repeat-owner 1)))
    (rulebase-insert-clause! rulebase (make-clause '(repeat-fact)))
    (let ((entry (first (cl-prolog::rulebase-entries rulebase))))
      (is (cl-prolog::%rulebase-retract-entry! rulebase entry))
      (is (not (cl-prolog::%rulebase-retract-entry! rulebase entry))))))

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

(deftest proof-search-falls-back-when-no-constraint-hook-is-installed ()
  "*constraint-post-unify-hook* decouples fact/rule matching and unification
from the finite-domain subsystem (installed only once fd-store.lisp loads);
verify the direct-unification fallback taken by an absent hook still
produces the normal proof-search result."
  (let ((rulebase (prolog ((likes alice bob))
                          ((admires ?x ?y) (fond-of ?x ?y))
                          ((fond-of alice bob))))
        (cl-prolog::*constraint-post-unify-hook* nil))
    (is-equal '(((?y . bob))) (query-prolog rulebase '(likes alice ?y)))
    (is-equal '(((?y . bob))) (query-prolog rulebase '(admires alice ?y)))
    (is-equal '(((?y . bob))) (query-prolog rulebase '(= (alice . ?y) (alice . bob))))))

