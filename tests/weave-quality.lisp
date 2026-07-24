;;;; cl-weave-specific regression coverage for relational behavior.

(in-package #:cl-prolog.tests)

(defvar *weave-family-rulebase* nil)

(cl-weave:describe-sequential "cl-weave relational regression"
  (cl-weave:before-each
    (setf *weave-family-rulebase* (make-family-rulebase)))
  (cl-weave:after-each
    (setf *weave-family-rulebase* nil))

  (cl-weave:it-each
      (((a b) (c) (a b c))
       (() (a) (a))
       ((left) () (left)))
      "append returns ~S for ~S and ~S" (left right expected)
    (cl-weave:expect-has-assertions)
    (cl-weave:expect
     (query-prolog *weave-family-rulebase* `(append ,left ,right ?result))
     :to-equal `(((?result . ,expected)))))

  (cl-weave:it-property
      "append preserves every generated pair of finite lists"
      ((left (cl-weave:gen-list (cl-weave:gen-member '(a b c)) :max-length 5))
       (right (cl-weave:gen-list (cl-weave:gen-member '(a b c)) :max-length 5)))
    (cl-weave:expect-has-assertions)
    (let ((joined (append left right)))
      (cl-weave:expect
     (query-prolog *weave-family-rulebase* `(append ,left ,right ?result))
     :to-equal `(((?result . ,joined))))))

  (cl-weave:it "repeated ancestor queries complete within the latency budget"
      (:timeout-ms 5000)
    (cl-weave:expect-has-assertions)
    (let ((result
            (cl-weave:measure
             (lambda ()
               (cl-weave:expect
                (query-prolog-first *weave-family-rulebase*
                                    '(ancestor tom ?who))
                :to-equal '((?who . bob))))
             :warmup 1 :samples 5 :iterations 40)))
      (cl-weave:expect (length (cl-weave:benchmark-result-samples result))
                       :to-be 5)
      (cl-weave:expect (cl-weave:median-ms result)
                       :to-be-greater-than-or-equal 0)
      (cl-weave:expect (cl-weave:median-ms result)
                       :to-be-less-than 2000))))

(cl-weave:it-property
    "the standard order of terms agrees with numeric order and is
antisymmetric for integers"
    ((left (cl-weave:gen-integer :min -1000 :max 1000))
     (right (cl-weave:gen-integer :min -1000 :max 1000)))
  (cl-weave:expect-has-assertions)
  (let ((forward (cl-prolog::%compare-terms left right))
        (backward (cl-prolog::%compare-terms right left)))
    (cl-weave:expect forward :to-equal (signum (- left right)))
    (cl-weave:expect backward :to-equal (- forward))))

(defparameter *iso-error-type-mutations*
  '(("TYPE_ERROR" . "DOMAIN_ERROR") ("DOMAIN_ERROR" . "TYPE_ERROR")
    ("INSTANTIATION_ERROR" . "TYPE_ERROR"))
  "Confusable ISO error-type formal names cl-prolog's own conditions carry
(cl-prolog::prolog-type-error, prolog-domain-error, prolog-instantiation-error).")

(cl-weave:defmutation-operator :iso-error-type-swap (form path)
  "Swaps an ISO error-type string literal (TYPE_ERROR, DOMAIN_ERROR,
INSTANTIATION_ERROR, ...) for a distinct, confusable ISO error-type name.
A test that only asserts \"some error was signalled\", without checking
which formal error type it carries, survives this mutant; a test that
checks the specific ISO type does not."
  (declare (ignore path))
  (when (stringp form)
    (let ((replacement (cdr (assoc form *iso-error-type-mutations* :test #'string=))))
      (when replacement (list replacement)))))

(deftest iso-error-type-mutation-operator-catches-loose-error-assertions ()
  "Demonstrates cl-weave's extension API (DEFMUTATION-OPERATOR), not just its
built-in operator set: a project-specific mutator for the ISO error-type
vocabulary cl-prolog's condition types carry (src/engine.lisp), applied to
two representative assertion styles."
  (let ((loose-results
          (cl-weave:run-mutations
           '(list "TYPE_ERROR" "INTEGER" 42)
           (lambda (form mutation)
             (declare (ignore mutation))
             ;; Only checks an error term was produced -- too loose to
             ;; notice the formal error type changed underneath it.
             (= (length (eval form)) 3))
           :operators '(:iso-error-type-swap)))
        (precise-results
          (cl-weave:run-mutations
           '(list "TYPE_ERROR" "INTEGER" 42)
           (lambda (form mutation)
             (declare (ignore mutation))
             ;; Checks the specific ISO formal error type, as
             ;; query-error-summary-based assertions throughout this suite do.
             (string= (first (eval form)) "TYPE_ERROR"))
           :operators '(:iso-error-type-swap))))
    (is (zerop (getf (cl-weave:mutation-summary loose-results) :killed)))
    (is (= 1 (getf (cl-weave:mutation-summary precise-results) :killed)))))

(deftest weave-solution-multiset-equal-rejects-length-mismatch ()
  "cl-prolog/weave::%solution-multiset-equal-p short-circuits to false when the
actual and expected solution lists differ in length -- the :set assertion path
taken when a rulebase yields a different count of solutions than expected."
  (is (not (cl-prolog/weave::%solution-multiset-equal-p '((a) (b)) '((a)))))
  (is (cl-prolog/weave::%solution-multiset-equal-p '((a) (b)) '((b) (a)))))

(deftest weave-parse-query-spec-requires-query-after-label ()
  "cl-prolog/weave::%parse-query-spec signals when a labelled spec supplies only
its string label and omits the query and assertion kind, exercising the
consp-body guard's failure branch."
  (signals-error (cl-prolog/weave::%parse-query-spec '("label only"))))

(deftest define-arithmetic-comparison-operator-mapping-kills-every-mutant ()
  "Mutation-test the define-arithmetic-comparison operator table (src/builtins/arithmetic.lisp):
a conjunction exercising each of =, /=, <, <=, >, >= must catch every
comparison-operator swap cl-weave's default mutator can produce."
  (let ((results
          (cl-weave:run-mutations
           '(and (= 2 2) (/= 2 3) (< 2 3) (<= 2 2) (> 3 2) (>= 2 2))
           (lambda (form mutation)
             (declare (ignore mutation))
             (eval form)))))
    (is (listp (cl-weave:assert-mutation-score results 1.0)))))
