;;;; Arithmetic-comparison error contract and prolog_flag builtin tests.

(in-package #:cl-prolog.tests)

(progn
  (deftest arithmetic-rejects-host-only-ratio-input ()
    (signals-error
      (query-prolog (make-rulebase) (list 'is '?result 1/2))))

  (deftest arithmetic-operator-lookup-does-not-intern-untrusted-names ()
    (let ((before (package-owned-symbol-count :keyword)))
      (is (eq :sqrt
              (cl-prolog::%arithmetic-operator-key
               (make-symbol "SQRT")
               cl-prolog::*unary-arithmetic-functions*)))
      (is (= pi
             (cl-prolog::%evaluate-arithmetic-expression
              (make-symbol "PI") nil)))
      (loop for index below 128
            for name = (format nil
                               "CL-PROLOG-INVALID-ARITHMETIC-~D" index)
            for operator = (make-symbol name)
            do (is (null (find-symbol name :keyword)))
               (signals-condition prolog-type-error
                 (cl-prolog::%require-arithmetic-function
                  operator 1 (list operator 1) nil))
               (signals-condition prolog-type-error
                 (cl-prolog::%evaluate-arithmetic-expression operator nil))
               (is (null (find-symbol name :keyword))))
      (is-equal before (package-owned-symbol-count :keyword))))

  (deftest arithmetic-power-rejects-an-oversized-exponent-or-result ()
    (signals-condition prolog-resource-error
      (query-prolog (make-rulebase) (list 'is '?result (list '** 2 5000))))
    (signals-condition prolog-resource-error
      (query-prolog (make-rulebase)
                    (list 'is '?result
                          (list '** (list '** 2 1000) 20)))))

  (deftest arithmetic-power-with-a-zero-exponent-skips-the-size-check ()
    (is-equal '(((?result . 1)))
              (query-prolog (make-rulebase) (list 'is '?result (list '** 5 0)))))

  (deftest arithmetic-rejects-an-improper-expression-list ()
    (signals-condition prolog-type-error
      (cl-prolog::%evaluate-arithmetic-expression (list* '+ 1 2) nil))))

(defun arithmetic-type-error-formal (expression)
  (handler-case
      (progn
        (query-prolog (make-rulebase) (list 'is '?result expression))
        (error "Expected an arithmetic type error for ~S" expression))
    (prolog-type-error (condition)
      (second (prolog-exception-term condition)))))

(deftest-table arithmetic-functions-validate-indicators-before-arguments ()
  (:equal
   (list (cl-prolog::%iso-atom "TYPE_ERROR")
         (cl-prolog::%iso-atom "EVALUABLE")
         (cl-prolog::%iso-term "/" 'unknown 1))
   (arithmetic-type-error-formal '(unknown 1)))
  (:equal
   (list (cl-prolog::%iso-atom "TYPE_ERROR")
         (cl-prolog::%iso-atom "EVALUABLE")
         (cl-prolog::%iso-term "/" '+ 3))
   (arithmetic-type-error-formal '(+ 1 2 3)))
  (:equal
   (list (cl-prolog::%iso-atom "TYPE_ERROR")
         (cl-prolog::%iso-atom "EVALUABLE")
         (cl-prolog::%iso-term "/" 'unknown 1))
   (arithmetic-type-error-formal '(unknown ?unbound))))

(deftest-queries prolog-flag-builtins ((make-rulebase))
  ((cl-prolog::current_prolog_flag bounded ?value) :ordered (((?value . cl-prolog:false))))
  ((cl-prolog::current_prolog_flag max_arity ?value) :ordered (((?value . cl-prolog::unbounded))))
  ((cl-prolog::current_prolog_flag unknown ?value) :ordered (((?value . cl-prolog.user-atoms::error))))
  ((cl-prolog::current_prolog_flag missing ?value) :fails)
  ((cl-prolog::current_prolog_flag ?name ?value) :set
   (((?name . cl-prolog::bounded) (?value . cl-prolog:false))
    ((?name . cl-prolog::max_arity) (?value . cl-prolog::unbounded))
    ((?name . cl-prolog::integer_rounding_function) (?value . cl-prolog::toward_zero))
    ((?name . cl-prolog::char_conversion) (?value . cl-prolog::off))
    ((?name . cl-prolog.user-atoms::debug) (?value . cl-prolog::off))
    ((?name . cl-prolog::unknown) (?value . cl-prolog.user-atoms::error))
    ((?name . cl-prolog::double_quotes) (?value . cl-prolog::codes))))
  ((cl-prolog::set_prolog_flag debug cl-prolog::on) :succeeds)
  ((cl-prolog::set_prolog_flag debug cl-prolog::off) :succeeds)
  ((cl-prolog::current_prolog_flag debug cl-prolog::off) :succeeds)
  ((cl-prolog::set_prolog_flag ?name on) :signals)
  ((cl-prolog::set_prolog_flag debug ?value) :signals)
  ((cl-prolog::set_prolog_flag missing on) :signals)
  ((cl-prolog::set_prolog_flag debug invalid) :signals)
  ((cl-prolog::set_prolog_flag bounded false) :signals)
  ((cl-prolog::current_prolog_flag 7 ?value) :signals))

(deftest prolog-flag-mutation-is-observable-within-one-rulebase ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (cl-prolog::set_prolog_flag debug cl-prolog::on) :succeeds)
    (assert-query rulebase (cl-prolog::current_prolog_flag debug cl-prolog::on) :succeeds)))

(deftest prolog-flag-values-are-rulebase-local ()
  (let ((changed (make-rulebase))
        (unchanged (make-rulebase)))
    (assert-query changed (cl-prolog::set_prolog_flag debug cl-prolog::on) :succeeds)
    (assert-query changed (cl-prolog::current_prolog_flag debug cl-prolog::on) :succeeds)
    (assert-query unchanged (cl-prolog::current_prolog_flag debug cl-prolog::off) :succeeds)))

(deftest prolog-flag-values-follow-rulebase-transactions ()
  (let* ((rulebase (make-rulebase))
         (transaction (cl-prolog::%copy-rulebase rulebase)))
    (assert-query transaction (cl-prolog::set_prolog_flag unknown cl-prolog:fail) :succeeds)
    (assert-query rulebase
                  (cl-prolog::current_prolog_flag unknown cl-prolog.user-atoms::error)
                  :succeeds)
    (cl-prolog::%replace-rulebase! rulebase transaction)
    (assert-query rulebase (cl-prolog::current_prolog_flag unknown cl-prolog:fail) :succeeds)))

(deftest-queries goal-shapes ((prolog ((ready))
                                      ((stuck ?in ?out) (= ?in ?out))))
  (ready                         :ordered (nil))
  ((ready)                       :ordered (nil))
  (missing                       :signals)
  (()                            :ordered (nil))
  (!                             :ordered (nil))
  ((! x)                         :signals)
  (((!) (= left left))           :ordered (nil))
  ((or ! (= left right))         :ordered (nil))
  (42                            :signals)
  (((1 2) (= a a))               :signals)
  ((= only-one)                  :signals)
  ((append a b)                  :signals)
  ((not x y)                     :signals)
  ((dcg-alt)                     :signals)
  ((dcg-token-match :noun stream) :signals))

(deftest-table control-builtins-report-iso-errors ()
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (term-builtin-error-summary '(throw ?ball)))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (term-builtin-error-summary '(once ?goal)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "CALLABLE" 42))
          (term-builtin-error-summary '(once 42)))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (term-builtin-error-summary '(not ?goal)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "CALLABLE" 42))
          (term-builtin-error-summary '(cl-prolog::|\\+| 42)))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (term-builtin-error-summary
           '(cl-prolog.user-atoms::ignore ?goal)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "CALLABLE" 42))
          (term-builtin-error-summary
           '(cl-prolog.user-atoms::ignore 42)))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (term-builtin-error-summary '(forall ?condition true)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "CALLABLE" 42))
          (term-builtin-error-summary '(forall true 42)))
  (:equal nil
          (term-builtin-error-summary '(forall fail 42)))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (term-builtin-error-summary
           '(setup_call_cleanup ?setup true true)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "CALLABLE" 42))
          (term-builtin-error-summary '(call_cleanup 42 true)))
  (:equal nil
          (term-builtin-error-summary
           '(setup_call_cleanup fail 42 43)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "CALLABLE" 43))
          (term-builtin-error-summary
           '(setup_call_cleanup true true 43)))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (term-builtin-error-summary
           '(if-then-else ?condition true true)))
  (:equal nil
          (term-builtin-error-summary
           '(if-then-else true true 42)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "CALLABLE" 42))
          (term-builtin-error-summary
           '(if-then-else fail true 42)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "CALLABLE" 42))
          (term-builtin-error-summary
           '(soft-if-then-else true 42 true)))
  (:equal nil
          (term-builtin-error-summary
           '(soft-if-then-else true true 42)))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (term-builtin-error-summary '(catch ?goal mismatch true)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "CALLABLE" 42))
          (term-builtin-error-summary
           '(catch (throw ball) ball 42))))

(deftest-queries arithmetic-builtins ((make-rulebase))
  ((is ?x (+ 2 (* 3 4)))         :ordered (((?x . 14))))
  ((is ?x (- 5))                 :ordered (((?x . -5))))
  ((is ?x (- 10 3))              :ordered (((?x . 7))))
  ((is ?x (/ 7 2))               :ordered (((?x . 3.5d0))))
  ((is ?x (/ 1 2))               :ordered (((?x . 0.5d0))))
  ((is ?x (mod 17 5))            :ordered (((?x . 2))))
  ((is ?x (abs -9))              :ordered (((?x . 9))))
  ((is ?x (// -7 3))             :ordered (((?x . -2))))
  ((is ?x (div -7 3))            :ordered (((?x . -3))))
  ((is ?x (rem -7 3))            :ordered (((?x . -1))))
  ((is ?x (mod -7 3))            :ordered (((?x . 2))))
  ((is ?x (sign -7))             :ordered (((?x . -1))))
  ((is ?x (min 7 3))             :ordered (((?x . 3))))
  ((is ?x (max 7 3))             :ordered (((?x . 7))))
  ((is ?x (floor (/ 7 3)))       :ordered (((?x . 2))))
  ((is ?x (ceiling (/ 7 3)))     :ordered (((?x . 3))))
  ((is ?x (truncate (/ -7 3)))   :ordered (((?x . -2))))
  ((is ?x (round (/ 5 2)))       :ordered (((?x . 2))))
  ((is ?x (** 2 8))              :ordered (((?x . 256))))
  ((is ?x (^ 3 3))               :ordered (((?x . 27))))
  ((is ?x (+ 3))                 :ordered (((?x . 3))))
  ((is ?x (|/\\| 6 3))          :ordered (((?x . 2))))
  ((is ?x (|\\/| 4 3))          :ordered (((?x . 7))))
  ((is ?x (xor 6 3))             :ordered (((?x . 5))))
  ((is ?x (|\\| 0))             :ordered (((?x . -1))))
  ((is ?x (|<<| 3 4))           :ordered (((?x . 48))))
  ((is ?x (|>>| -16 2))         :ordered (((?x . -4))))
  ((is ?x (float 3))            :ordered (((?x . 3.0d0))))
  ((is ?x (float_integer_part -2.75d0)) :ordered (((?x . -2.0d0))))
  ((is ?x (float_fractional_part -2.75d0)) :ordered (((?x . -0.75d0))))
  ((is ?x signum)                :signals)
  ((is ?x (signum 9))           :ordered (((?x . 1))))
  ((|=:=| pi 3.141592653589793d0) :succeeds)
  ((|=:=| (sqrt 9) 3)            :succeeds)
  ((|=:=| (exp 0) 1)             :succeeds)
  ((|=:=| (log 1) 0)             :succeeds)
  ((|=:=| (sin 0) 0)             :succeeds)
  ((|=:=| (cos 0) 1)             :succeeds)
  ((|=:=| (tan 0) 0)             :succeeds)
  ((|=:=| (asin 0) 0)            :succeeds)
  ((|=:=| (acos 1) 0)            :succeeds)
  ((|=:=| (atan 0) 0)            :succeeds)
  ((|=:=| (atan 0 -1) pi)        :succeeds)
  ((is 3 (+ 1 2))                :succeeds)
  ((is 4 (+ 1 2))                :fails)
  ((|=:=| (+ 1 2) 3)             :succeeds)
  ((|=:=| 3 4)                   :fails)
  ((|=\\=| 3 4)                  :succeeds)
  ((|=\\=| 3 (+ 1 2))            :fails)
  ((< 2 3)                       :succeeds)
  ((< 3 2)                       :fails)
  ((=< 3 3)                      :succeeds)
  ((> 4 3)                       :succeeds)
  ((>= 4 4)                      :succeeds)
  ((cl-prolog::between 2 4 ?x)   :ordered (((?x . 2)) ((?x . 3)) ((?x . 4))))
  ((cl-prolog::between -2 0 ?x)  :ordered (((?x . -2)) ((?x . -1)) ((?x . 0))))
  ((cl-prolog::between 2 4 3)    :succeeds)
  ((cl-prolog::between 2 4 5)    :fails)
  ((cl-prolog::between 4 2 ?x)   :fails)
  ((cl-prolog::succ 2 ?x)        :ordered (((?x . 3))))
  ((cl-prolog::succ ?x 3)        :ordered (((?x . 2))))
  ((cl-prolog::succ 2 3)         :succeeds)
  ((cl-prolog::succ 2 4)         :fails)
  ((cl-prolog::succ ?x 0)        :fails)
  ((cl-prolog::between ?low 4 ?x) :signals)
  ((cl-prolog::between 1 4.0 ?x) :signals)
  ((cl-prolog::between 1 4 atom) :signals)
  ((cl-prolog::succ ?x ?y)       :signals)
  ((cl-prolog::succ -1 ?x)       :signals)
  ((cl-prolog::succ ?x -1)       :signals)
  ((cl-prolog::succ 1.0 ?x)      :signals)
  ((and (= ?x 4) (is ?y (+ ?x 1))) :ordered (((?x . 4) (?y . 5))))
  ((is ?x (+ ?unbound 1))        :signals)
  ((is ?x (+ atom 1))            :signals)
  ((is ?x (/ 1 0))               :signals)
  ((is ?x (// 1 0))              :signals)
  ((is ?x (rem 1.0 2))           :signals)
  ((is ?x (mod 1 0))             :signals)
  ((is ?x (sqrt -1))             :signals)
  ((is ?x (** -1 0.5d0))        :signals)
  ((is ?x (log 0))               :signals)
  ((is ?x (|/\\| 1.0 1))        :signals)
  ((is ?x (|<<| 1 1.5))         :signals))

(deftest arithmetic-evaluation-error-reports-expression-and-reason ()
  (let ((condition
          (handler-case
              (progn
                (query-prolog (make-rulebase) '(is ?x (sqrt -1)))
                (error "Expected an ARITHMETIC-EVALUATION-ERROR"))
            (arithmetic-evaluation-error (condition) condition))))
    (is-equal '(sqrt -1) (arithmetic-error-expression condition))
    (is (search "square root" (arithmetic-error-reason condition)))))
