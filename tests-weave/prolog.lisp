;;;; cl-weave test suites for the public cl-prolog engine surface.

(in-package #:cl-prolog/weave-tests)

;;; ------------------------------------------------------------------
;;; Fixtures and helpers
;;; ------------------------------------------------------------------

(defun family ()
  "A small family rulebase used across the relation suites."
  (prolog
    ((parent tom bob))
    ((parent bob alice))
    ((parent alice eve))
    ((ancestor ?x ?y) (parent ?x ?y))
    ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y))
    ((grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))))

(defun canonical (solutions)
  "Return SOLUTIONS in an order-insensitive canonical form.

Each solution is an alist of (VARIABLE . VALUE) bindings.  Both the bindings
inside a solution and the solutions themselves are sorted by printed form so
order-independent result sets compare with EQUAL."
  (labels ((sort-key (object) (prin1-to-string object)))
    (sort (mapcar (lambda (solution)
                    (sort (copy-list solution) #'string<
                          :key (lambda (binding) (sort-key (car binding)))))
                  solutions)
          #'string< :key #'sort-key)))

;;; ------------------------------------------------------------------
;;; Unification
;;; ------------------------------------------------------------------

(describe "unification"
  (it "binds a variable to a constant"
    (multiple-value-bind (env ok) (unify '?x 'alice)
      (expect ok :to-be-truthy)
      (expect (logic-substitute '?x env) :to-equal 'alice)))

  (it "unifies compound terms structurally"
    (multiple-value-bind (env ok) (unify '(pair ?x ?y) '(pair left right))
      (expect ok :to-be-truthy)
      (expect (logic-substitute '(pair ?x ?y) env) :to-equal '(pair left right))))

  (it "propagates a shared variable across a term"
    (multiple-value-bind (env ok) (unify '(pair ?x ?x) '(pair left ?y))
      (expect ok :to-be-truthy)
      (expect (logic-substitute '?y env) :to-equal 'left)))

  (it "fails on mismatched constants"
    (expect (nth-value 1 (unify 'a 'b)) :to-be-falsy))

  (it "rejects an occurs-check cycle"
    (expect (nth-value 1 (unify '?x '(wrap ?x))) :to-be-falsy)))

;;; ------------------------------------------------------------------
;;; Family relations (facts + recursive rules)
;;; ------------------------------------------------------------------

(describe "family relations"
  (it "enumerates every ancestor of tom in search order"
    (expect (query-prolog (family) '(ancestor tom ?who))
            :to-equal '(((?who . bob)) ((?who . alice)) ((?who . eve)))))

  (it "resolves the descendants that reach a fixed ancestor"
    (expect (canonical (query-prolog (family) '(ancestor ?x eve)))
            :to-equal (canonical '(((?x . tom)) ((?x . bob)) ((?x . alice))))))

  (it "returns only the first solution with query-prolog-first"
    (expect (query-prolog-first (family) '(ancestor ?x bob))
            :to-equal '((?x . tom))))

  (it "derives grandparents through the two-step rule"
    (expect (query-prolog (family) '(grandparent tom ?z))
            :to-equal '(((?z . alice)))))

  (it "reports a provable goal as succeeding"
    (expect (prolog-succeeds-p (family) '(ancestor tom eve)) :to-be-truthy))

  (it "reports an unprovable goal as failing"
    (expect (prolog-succeeds-p (family) '(ancestor eve tom)) :to-be-falsy)))

;;; ------------------------------------------------------------------
;;; List builtins
;;; ------------------------------------------------------------------

(describe "list builtins"
  (it "enumerates the members of a list"
    (expect (canonical (query-prolog (make-rulebase) '(member ?x (bob alice))))
            :to-equal (canonical '(((?x . bob)) ((?x . alice))))))

  (it "appends two proper lists"
    (expect (query-prolog (make-rulebase) '(append (a b) (c) ?xs))
            :to-equal '(((?xs a b c)))))

  (it "splits a list into every prefix/suffix pair"
    (expect (query-prolog (make-rulebase) '(append ?left ?right (a b)))
            :to-equal '(((?left) (?right a b))
                        ((?left a) (?right b))
                        ((?left a b) (?right)))))

  (it "reverses a list"
    (expect (query-prolog (make-rulebase) '(reverse (a b c) ?x))
            :to-equal '(((?x c b a)))))

  (it "computes the length of a list"
    (expect (query-prolog (make-rulebase) '(length (a b c) ?n))
            :to-equal '(((?n . 3))))))

;;; ------------------------------------------------------------------
;;; Control-flow builtins
;;; ------------------------------------------------------------------

(describe "control-flow builtins"
  (it "unifies with ="
    (expect (query-prolog (make-rulebase) '(= ?x left))
            :to-equal '(((?x . left)))))

  (it "fails = on distinct constants"
    (expect (query-prolog (make-rulebase) '(= left right)) :to-be-null))

  (it "enumerates both branches of a disjunction"
    (expect (canonical (query-prolog (make-rulebase) '(or (= ?x left) (= ?x right))))
            :to-equal (canonical '(((?x . left)) ((?x . right))))))

  (it "threads bindings through a conjunction"
    (expect (query-prolog (family) '(and (parent tom ?x) (parent ?x alice)))
            :to-equal '(((?x . bob))))))

;;; ------------------------------------------------------------------
;;; Goal validation
;;; ------------------------------------------------------------------

(describe "goal validation"
  (it "signals on a builtin called with the wrong arity"
    (expect (lambda () (query-prolog (make-rulebase) '(= only-one)))
            :to-throw 'error))

  (it "signals on a non-goal term"
    (expect (lambda () (query-prolog (make-rulebase) '42))
            :to-throw 'error)))
