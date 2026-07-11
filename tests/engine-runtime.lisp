;;;; Runtime, control, and internal engine behavior tests.

(in-package #:cl-prolog.tests)

(cl-prolog::define-builtin (test-twice input output)
    (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (let ((value (logic-substitute input environment)))
    (when (numberp value)
      (cl-prolog::%unify-emit output (* 2 value) environment emit))))

(cl-prolog::define-builtin ((test-collect test-collect-alias) output &rest arguments)
    (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (cl-prolog::%unify-emit output (copy-list arguments) environment emit))

(deftest-queries cut-prunes-clause-alternatives
    ((prolog
      ((choice left))
      ((choice right))
      ((pick ?x) (choice ?x) !)
      ((pick fallback) (choice left))))
  ((pick ?x) => (((?x . left))))
  (((choice ?x) !) => (((?x . left))))
  ((and (choice ?x) !) => (((?x . left))))
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

(deftest-queries depth-bound-terminates-recursion
    ((prolog
      ((loop-forever) (loop-forever))))
  ((loop-forever) => () :max-depth 16)
  ((loop-forever) :fails :max-depth 16))

(deftest foreign-predicate-hook ()
  (let ((rb (make-family-rulebase))
        (*recorded-colors* '()))
    (assert-query rb (accepted :ok) => (nil))
    (assert-query rb (accepted :ng) => ())
    (assert-query rb (colored ?x) => (((?x . tom))))
    (is-equal '(tom) *recorded-colors*)))

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
    (assert-query rb (ancestor tom ?who) => () :max-depth -1)))

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

(deftest define-builtin-supports-aliases-and-rest-arguments ()
  (is-equal '(((?arguments . (a b c))))
            (query-prolog (make-rulebase)
                          '(test-collect-alias ?arguments a b c))))

(deftest define-builtin-macroexpand-registers-single-name ()
  (with-macroexpansion (expansion
                        '(cl-prolog::define-builtin (twice input output)
                           (rulebase environment depth emit)
                           (declare (ignore rulebase depth))
                           (cl-prolog::%unify-emit output
                                                   (* 2 (logic-substitute input environment))
                                                   environment
                                                   emit)))
    (is (%tree-contains-p expansion 'defmethod))
    (is (%tree-contains-p expansion 'cl-prolog::%goal-solver))
    (is (%tree-contains-p expansion 'eql))
    (is (%tree-contains-p expansion 'twice))))

(deftest define-builtin-macroexpand-registers-aliases-and-rest ()
  (with-macroexpansion (expansion
                        '(cl-prolog::define-builtin ((collect collect-alias) output &rest arguments)
                           (rulebase environment depth emit)
                           (declare (ignore rulebase depth))
                           (cl-prolog::%unify-emit output
                                                   (copy-list arguments)
                                                   environment
                                                   emit)))
    (is (%tree-contains-p expansion 'defmethod))
    (is (%tree-contains-p expansion 'cl-prolog::%goal-solver))
    (is (%tree-contains-p expansion 'collect))
    (is (%tree-contains-p expansion 'collect-alias))))

(deftest-table invalid-goal-error-reports-context ()
  (:equal '(= a)
          (handler-case
              (progn
                (query-prolog (make-rulebase) '(= a))
                (error "Expected an INVALID-GOAL-ERROR"))
            (invalid-goal-error (condition)
              (invalid-goal-error-goal condition))))
  (:is (handler-case
          (progn
            (query-prolog (make-rulebase) '(= a))
            (error "Expected an INVALID-GOAL-ERROR"))
        (invalid-goal-error (condition)
          (search "expects" (princ-to-string condition))))))

(deftest-table query-normalization-internals ()
  (:equal '() (cl-prolog::%normalize-query nil))
  (:equal '((parent tom bob))
          (cl-prolog::%normalize-query '(parent tom bob)))
  (:equal '((parent tom bob) (parent bob alice))
          (cl-prolog::%normalize-query '((parent tom bob) (parent bob alice))))
  (:equal '(!) (cl-prolog::%normalize-query '!))
  (:equal nil (cl-prolog::%with-cut-barrier :ok))
  (:is (cl-prolog::%with-cut-barrier (cl-prolog::%propagate-cut)))
  (:equal '(unless (minusp depth) (run))
          (with-macroexpansion (expansion '(cl-prolog::%with-depth-guard depth (run)))
            expansion)))
