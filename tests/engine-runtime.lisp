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

;; The recursive argument keeps growing, so variant tabling cannot close a
;; fixed point and the explicit depth budget must fire.  (A plain P :- P
;; loop is answered finitely by tabling and would fail instead of signal.)
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
