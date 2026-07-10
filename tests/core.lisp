;;;; Package surface, data model, unification, and rule-DSL tests.

(in-package #:fx.prolog.tests)

(deftest package-surface ()
  (is (find-package "FX.PROLOG"))
  (dolist (name '("PROLOG" "QUERY-PROLOG" "MAP-PROLOG-SOLUTIONS" "DEFINE-BUILTIN"
                  "DEF-RULE" "PHRASE" "UNIFY" "LOGIC-SUBSTITUTE"
                  "FRESH-LOGIC-VARIABLE" "INVALID-GOAL-ERROR"))
    (is (eq :external (nth-value 1 (find-symbol name "FX.PROLOG")))
        (format nil "Expected exported symbol ~A" name)))
  (dolist (name '("SUBSTITUTE-TERM" "*MAX-PROOF-DEPTH*" "UNIFY-FAILED-P"
                  "WHEN-UNIFY-SUCCEEDS" "WHEN-UNIFY-FAILS"
                  "QUERY-PROLOG-CPS" "PROLOG-SUCCEEDS-P-CPS"))
    (is (not (eq :external (nth-value 1 (find-symbol name "FX.PROLOG"))))
        (format nil "Legacy symbol ~A must not be exported" name))))

(deftest logic-variables ()
  (is (logic-var-p '?x))
  (is (logic-var-p (fresh-logic-variable)))
  (is (not (logic-var-p :?keyword)))
  (is (not (logic-var-p 'plain)))
  (is (not (logic-var-p (intern ""))))
  (is (not (logic-var-p 42))))

(deftest unification-protocol ()
  ;; success extends the environment
  (multiple-value-bind (env ok)
      (unify '(pair ?x ?y) '(pair left right))
    (is ok)
    (is-equal 'left (logic-substitute '?x env))
    (is-equal 'right (logic-substitute '?y env)))
  ;; failure returns (VALUES NIL NIL)
  (multiple-value-bind (env ok)
      (unify '(tag a) '(tag b))
    (is (not ok))
    (is (null env)))
  ;; the occurs check rejects cyclic bindings on both sides
  (is (not (nth-value 1 (unify '?x '(wrap ?x)))))
  (is (not (nth-value 1 (unify '(wrap ?x) '?x))))
  ;; non-cons equality falls back to EQUAL
  (is (nth-value 1 (unify "same" "same")))
  (is (nth-value 1 (unify 7 7)))
  (is (not (nth-value 1 (unify 7 8))))
  ;; dotted pairs unify structurally
  (multiple-value-bind (env ok)
      (unify '(:outer (:inner . ?side) ?side) '(:outer (:inner . :buy) ?other))
    (is ok)
    (is-equal '(:outer (:inner . :buy) :buy)
              (logic-substitute '(:outer (:inner . ?side) ?side) env)))
  ;; bound variables walk transitively
  (multiple-value-bind (env ok)
      (unify '?x '?y '((?y . done)))
    (is ok)
    (is-equal 'done (logic-substitute '?x env))))

(deftest rulebase-data-model ()
  (let ((rb (prolog
              ((parent tom bob))
              ((parent bob alice))
              ((ancestor ?x ?y) (parent ?x ?y))
              ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y)))))
    (is (rulebase-p rb))
    (is-equal 2 (length (rulebase-facts rb)))
    (is-equal 2 (length (rulebase-rules rb)))
    (let ((fact (first (rulebase-facts rb)))
          (rule (first (rulebase-rules rb))))
      (is-equal 'parent (fact-predicate fact))
      (is-equal '(tom bob) (fact-args fact))
      (is-equal '(ancestor ?x ?y) (rule-head rule))
      (is-equal '((parent ?x ?y)) (rule-body rule))))
  (let ((fact (make-fact :predicate 'lonely))
        (rule (make-rule))
        (rb (make-rulebase)))
    (is-equal '() (fact-args fact))
    (is-equal '() (rule-head rule))
    (is-equal '() (rule-body rule))
    (is-equal '() (rulebase-facts rb))
    (is-equal '() (rulebase-rules rb)))
  (is (signals-error (macroexpand-1 '(prolog (:facts (parent tom bob)))))
      "Invalid PROLOG clause should fail at expansion time")
  (is (signals-error (macroexpand-1 '(prolog (42))))
      "A clause head must be a list")
  (is (signals-error (macroexpand-1 '(prolog ready)))
      "A clause must be a list"))

(deftest mutable-global-rulebase ()
  (with-clean-global-rulebase
    (assert-fact! *global-rulebase* (make-fact :predicate 'planet :args '(earth)))
    (assert-rule! *global-rulebase*
                  (make-rule :head '(inhabited ?x) :body '((planet ?x))))
    (def-rule (warm ?x) (planet ?x))
    (is-equal '(nil) (query-prolog *global-rulebase* '(planet earth)))
    (is-equal '(nil) (query-prolog *global-rulebase* '(inhabited earth)))
    (is-equal '(nil) (query-prolog *global-rulebase* '(warm earth)))
    (clear-global-rulebase!)
    (is-equal '() (query-prolog *global-rulebase* '(planet earth)))
    (is-equal '() (rulebase-rules *global-rulebase*))))

(deftest extend-rulebase-shadowing ()
  (let* ((base (prolog ((parent tom bob))))
         (extended (extend-rulebase base
                     ((parent bob alice))
                     ((ancestor ?x ?y) (parent ?x ?y))
                     ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y)))))
    (is-equal '(nil) (query-prolog base '(parent tom bob)))
    (is-equal '() (query-prolog base '(parent bob alice)))
    (is-equal '(((?x . tom))) (query-prolog extended '(ancestor ?x bob)))
    (is-equal '(((?x . tom))) (query-prolog *macro-rulebase* '(ancestor ?x bob)))))

(deftest when-guards-compile-to-closures ()
  (let ((rb (prolog
              ((score alice 42))
              ((score bob 7))
              ((high-score ?who ?n) (score ?who ?n) (:when (> ?n 10)))
              ((constant-yes) (:when t))
              ((constant-no) (:when nil)))))
    ;; guard goals carry a function, not an expression
    (let* ((rule (find '(high-score ?who ?n) (rulebase-rules rb)
                       :key #'rule-head :test #'equal))
           (guard (second (rule-body rule))))
      (is (functionp (second guard)) "The :when guard must be compiled to a closure")
      (is-equal '(?n) (cddr guard)))
    (is-equal '(((?who . alice) (?n . 42)))
              (query-prolog rb '(high-score ?who ?n)))
    (is (prolog-succeeds-p rb '(constant-yes)))
    (is (not (prolog-succeeds-p rb '(constant-no))))))

(deftest when-guard-shape-is-strict ()
  ;; only (:when EXPR) is a compiled guard; other shapes stay plain data
  (is (fx.prolog::%when-guard-p '(:when (> ?n 1))))
  (is (not (fx.prolog::%when-guard-p '(:when))))
  (is (not (fx.prolog::%when-guard-p '(:when f ?x))))
  (is (not (fx.prolog::%when-guard-p 'ready)))
  ;; bare-symbol body goals pass through the clause compiler untouched
  (let ((rb (prolog
              ((ready))
              ((launchable) ready))))
    (is-equal '(nil) (query-prolog rb '(launchable)))))

(deftest guards-nest-inside-control-goals ()
  (let ((rb (prolog
              ((n 1)) ((n 2)) ((n 3))
              ((odd-n ?x) (n ?x) (not (:when (evenp ?x))))
              ((tagged ?x ?tag)
               (n ?x)
               (or ((:when (= ?x 1)) (= ?tag one))
                   (= ?tag other))))))
    (is-equal '(((?x . 1)) ((?x . 3))) (query-prolog rb '(odd-n ?x)))
    (is-same-set '(((?tag . one)) ((?tag . other)))
                 (query-prolog rb '(tagged 1 ?tag)))
    (is-equal '(((?tag . other)))
              (query-prolog rb '(tagged 2 ?tag)))))

(deftest def-rule-compiles-when-guards ()
  (with-clean-global-rulebase
    (assert-fact! *global-rulebase* (make-fact :predicate 'n :args '(4)))
    (assert-fact! *global-rulebase* (make-fact :predicate 'n :args '(5)))
    (is-equal '(even ?x) (def-rule (even ?x) (n ?x) (:when (evenp ?x))))
    (is-equal '(((?x . 4))) (query-prolog *global-rulebase* '(even ?x)))))

(deftest with-prolog-query-and-match ()
  (let ((rb (make-family-rulebase))
        (seen nil))
    (with-prolog-query (?x) (rb '(ancestor ?x bob))
      (setf seen ?x))
    (is-equal 'tom seen)
    (with-prolog-query (?x) (rb '(ancestor eve tom))
      (setf seen :should-not-run))
    (is-equal 'tom seen)
    (is-equal :direct
              (prolog-match rb
                ((parent tom bob) :direct)
                ((ancestor eve tom) :wrong)
                ((ancestor tom eve) :fallback)))
    (is-equal nil
              (prolog-match rb
                ((parent eve tom) :never)))))
