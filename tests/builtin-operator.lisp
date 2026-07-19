;;;; ISO operator builtin contract.

(in-package #:cl-prolog.tests)

(defun operator-error-type (goal)
  (handler-case
      (progn (query-prolog (make-rulebase) goal) nil)
    (prolog-runtime-error (condition)
      (type-of condition))))

(deftest operator-declaration-and-removal ()
  (let ((rulebase (make-rulebase)))
    (is (prolog-succeeds-p
         rulebase '(cl-prolog::op 450 cl-prolog::yfx custom-operator)))
    (is (prolog-succeeds-p
         rulebase '(cl-prolog::current_op 450 cl-prolog::yfx custom-operator)))
    (is (prolog-succeeds-p
         rulebase '(cl-prolog::op 0 cl-prolog::yfx custom-operator)))
    (is (not (prolog-succeeds-p
              rulebase
              '(cl-prolog::current_op 450 cl-prolog::yfx custom-operator))))))

(deftest operator-list-update-is-transactional ()
  (let ((rulebase (make-rulebase)))
    (is (eq 'prolog-type-error
            (handler-case
                (progn
                  (query-prolog
                   rulebase
                   '(cl-prolog::op 450 cl-prolog::yfx (valid-name 7)))
                  nil)
              (prolog-runtime-error (condition) (type-of condition)))))
    (is (not (prolog-succeeds-p
              rulebase '(cl-prolog::current_op 450 cl-prolog::yfx valid-name))))))

(progn
  (deftest current-op-enumerates-with-cps-backtracking ()
    (let* ((rulebase (make-rulebase))
           (expected (length (cl-prolog::%operator-table-current
                              (cl-prolog::rulebase-operator-table rulebase))))
           (solutions (query-prolog
                       rulebase
                       '(cl-prolog::current_op ?priority ?specifier ?name))))
      (is-equal expected (length solutions))))

  (deftest operator-specifier-lookup-does-not-intern-untrusted-names ()
    (labels ((keyword-symbol-count ()
               (let ((count 0)
                     (package (find-package :keyword)))
                 (do-symbols (symbol package count)
                   (when (eq (symbol-package symbol) package)
                     (incf count))))))
      (let ((before (keyword-symbol-count))
            (operation (cl-prolog::%iso-atom "OP")))
        (loop for name in '("FX" "FY" "XF" "YF" "XFX" "XFY" "YFX")
              for expected in cl-prolog::+operator-specifiers+
              do (is (eq expected
                         (cl-prolog::%operator-specifier
                          (make-symbol name) nil operation))))
        (loop for index below 128
              for name = (format nil
                                 "CL-PROLOG-INVALID-SPECIFIER-~D" index)
              do (is (null (find-symbol name :keyword)))
                 (signals-condition prolog-domain-error
                   (cl-prolog::%operator-specifier
                    (make-symbol name) nil operation))
                 (is (null (find-symbol name :keyword))))
        (is-equal before (keyword-symbol-count))))))

(deftest-table operator-builtins-report-iso-errors ()
  (:equal 'prolog-instantiation-error
          (operator-error-type '(cl-prolog::op ?priority cl-prolog::yfx name)))
  (:equal 'prolog-type-error
          (operator-error-type '(cl-prolog::op 1.5 cl-prolog::yfx name)))
  (:equal 'prolog-domain-error
          (operator-error-type '(cl-prolog::op 1201 cl-prolog::yfx name)))
  (:equal 'prolog-domain-error
          (operator-error-type '(cl-prolog::op 500 cl-prolog::invalid name)))
  (:equal 'prolog-instantiation-error
          (operator-error-type '(cl-prolog::op 500 cl-prolog::yfx ?name)))
  (:equal 'prolog-type-error
          (operator-error-type '(cl-prolog::current_op bad ?specifier ?name)))
  (:equal 'prolog-permission-error
          (operator-error-type '(cl-prolog::op 500 cl-prolog::xfy cl-prolog::|,|))))

(deftest-queries char-conversion-builtins ((make-rulebase))
  ((cl-prolog::current_char_conversion ?from ?to) :fails)
  ((cl-prolog::char_conversion cl-prolog::|a| cl-prolog::|b|) :succeeds)
  ((cl-prolog::char_conversion cl-prolog::|x| cl-prolog::|y|) :succeeds)
  ((cl-prolog::current_char_conversion cl-prolog::|b| ?to) :fails)
  ;; Mapping a character to itself removes its conversion.
  ((cl-prolog::char_conversion cl-prolog::|a| cl-prolog::|a|) :succeeds)
  ((cl-prolog::current_char_conversion cl-prolog::|a| ?to) :fails)
  ((cl-prolog::char_conversion ?from cl-prolog::|b|) :signals)
  ((cl-prolog::char_conversion ab cl-prolog::|b|) :signals)
  ((cl-prolog::char_conversion cl-prolog::|a| 7) :signals))

(deftest char-conversion-enumeration-reflects-one-rulebase ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase
                  (cl-prolog::char_conversion cl-prolog::|a| cl-prolog::|b|)
                  :succeeds)
    (assert-query rulebase
                  (cl-prolog::char_conversion cl-prolog::|x| cl-prolog::|y|)
                  :succeeds)
    (assert-query rulebase (cl-prolog::current_char_conversion ?from ?to)
                  => (((?from . cl-prolog::|a|) (?to . cl-prolog::|b|))
                      ((?from . cl-prolog::|x|) (?to . cl-prolog::|y|))))
    (assert-query rulebase (cl-prolog::current_char_conversion cl-prolog::|a| ?to)
                  => (((?to . cl-prolog::|b|))))))
