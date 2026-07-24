;;;; Foreign-predicate dispatch and define-builtin registration/macroexpansion
;;;; tests.

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

(deftest foreign-predicate-cps-solutions ()
  (let ((rb (make-family-rulebase))
        (*recorded-colors* '()))
    (assert-query rb (accepted :ok) :ordered (nil))
    (assert-query rb (accepted :ng) :ordered ())
    (assert-query rb (colored ?x) :ordered (((?x . tom))))
    (is-equal '(tom) *recorded-colors*)
    (assert-query rb (foreign-choice ?value)
                  :ordered (((?value . left)) ((?value . right))))))

(deftest foreign-predicate-dispatches-by-exact-indicator ()
  (let ((rb (prolog
              ((foreign-choice fallback zero))
              ((foreign-choice fallback)))))
    ;; No FOREIGN-CHOICE/0 solver or clause exists, so ISO requires an
    ;; existence error rather than a silent failure.
    (assert-query rb (foreign-choice) :signals)
    (assert-query rb (foreign-choice fallback zero) :ordered (nil))
    (assert-query rb (foreign-choice fallback) :ordered ())))

(deftest registered-foreign-predicate-is-authoritative ()
  (let ((rb (prolog ((foreign-choice clause-solution)))))
    (assert-query rb (foreign-choice ?value)
                  :ordered (((?value . left)) ((?value . right))))))

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

(deftest define-foreign-predicate-rejects-a-variadic-argument-list ()
  (signals-error
    (macroexpand-1
     '(cl-prolog::define-foreign-predicate
       (foreign-variadic-example value &rest more)
       (rulebase environment depth emit)
       (declare (cl:ignore rulebase environment depth more))
       (funcall emit environment)))))

(deftest iso-builtin-macro-treats-any-non-raw-compound-argument-as-resolvable ()
  (let ((expansion (macroexpand-1
                     '(cl-prolog::define-iso-builtin
                       (test_iso_builtin_arg_shape (value :other)) "TEST"
                       nil))))
    (is (search "RESOLVED-VALUE" (format nil "~S" expansion)))))

(deftest-table runtime-when-guards-take-functions ()
  (:equal '(((?x . bob)))
          (query-prolog (make-family-rulebase)
                        (list 'and
                              '(parent tom ?x)
                              (list :when (lambda (x) (eq x 'bob)) '?x))))
  (:signals (query-prolog (make-family-rulebase) '(:when (equal 'bob 'bob)))))

(deftest query-limits-and-streaming ()
  (let ((rb (make-family-rulebase)))
    (assert-query rb (ancestor tom ?who) :ordered (((?who . bob))) :limit 1)
    (is-equal 2 (length (query-prolog rb (quote (ancestor tom ?who)) :limit 2)))
    (dolist (limit (quote (0 -1 1.5 "1")))
      (handler-case
          (progn
            (query-prolog rb (quote (ancestor tom ?who)) :limit limit)
            (error "Expected a TYPE-ERROR"))
        (type-error (condition)
          (is-equal limit (type-error-datum condition))
          (is-equal (quote (or null (integer 1 *)))
                    (type-error-expected-type condition))))
      (signals-condition
       type-error
       (map-prolog-solutions (lambda (solution) (declare (ignore solution)))
                             rb (quote (ancestor tom ?who)) :limit limit)))
    (signals-condition
     type-error
     (query-prolog-first rb (quote (ancestor tom ?who)) :limit 0))
    (signals-condition
     program-error
     (query-prolog rb (quote (ancestor tom ?who)) :limti 1))
    (signals-condition
     program-error
     (query-prolog rb (quote (ancestor tom ?who)) :limit))
    (is-equal (quote (((?who . bob)) t))
              (multiple-value-list
               (query-prolog-first rb (quote (ancestor tom ?who)) :limit 2)))
    (let ((seen (quote ())))
      (map-prolog-solutions (lambda (solution) (push solution seen))
                            rb (quote (ancestor tom ?who)) :limit 2)
      (is-equal (quote (((?who . bob)) ((?who . alice)))) (reverse seen)))
    (let ((raw (query-prolog rb (quote (ancestor ?x bob)) :project nil)))
      (is-equal 1 (length raw))
      (is (assoc (quote ?x) (first raw))))
    (handler-case
        (progn
          (query-prolog rb (quote (ancestor tom ?who)) :max-depth -1)
          (error "Expected an INVALID-MAX-DEPTH-ERROR"))
      (invalid-max-depth-error (condition)
        (is-equal -1 (invalid-max-depth-error-value condition))))
    (is (signals-error
         (prolog-succeeds-p rb (quote (ancestor tom ?who)) :max-depth 1.5)))))

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
                  :ordered (((?p . 1) (?q . 2))))))

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

(progn
  (deftest define-builtin-macroexpand-registers-aliases-and-rest ()
    (with-macroexpansion (expansion
                          (quote (cl-prolog::define-builtin
                                     ((collect collect-alias) output &rest arguments)
                                     (rulebase environment depth emit)
                                   (declare (cl:ignore rulebase output depth)
                                            (cl:ignorable environment arguments)
                                            (optimize speed))
                                   (declare (cl:ignore emit)
                                            (type list arguments))
                                   (cl-prolog::%unify-emit output
                                                           (copy-list arguments)
                                                           environment
                                                           emit))))
      (is (%tree-contains-p expansion (quote cl-prolog::%register-builtin-solver!)))
      (is (%tree-contains-p expansion (quote eval-when)))
      (is (%tree-contains-p expansion (quote collect)))
      (is (%tree-contains-p expansion (quote collect-alias)))
      (is (%tree-contains-p expansion
                            (quote (declare (cl:ignore output)
                                            (cl:ignorable arguments)
                                            (optimize speed)))))
      (is (%tree-contains-p expansion
                            (quote (declare (type list arguments)))))
      (is (not (%tree-contains-p expansion
                                 (quote (declare (cl:ignore rulebase output depth))))))
      (is (not (%tree-contains-p expansion
                                 (quote (declare (cl:ignore emit))))))))

  (deftest define-builtin-preserves-declaration-for-shadowing-argument ()
    (with-macroexpansion (expansion
                          (quote (cl-prolog::define-builtin (shadowed environment)
                                     (rulebase environment depth emit)
                                   (declare (cl:ignore rulebase environment depth))
                                   (funcall emit nil))))
      (is (%tree-contains-p expansion
                            (quote (declare (cl:ignore environment))))))))
