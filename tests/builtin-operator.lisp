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

(deftest current-op-enumerates-with-cps-backtracking ()
  (let* ((rulebase (make-rulebase))
         (expected (length (cl-prolog::%operator-table-current
                            (cl-prolog::rulebase-operator-table rulebase))))
         (solutions (query-prolog
                     rulebase
                     '(cl-prolog::current_op ?priority ?specifier ?name))))
    (is-equal expected (length solutions))))

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
