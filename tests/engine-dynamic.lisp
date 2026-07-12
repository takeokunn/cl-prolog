(in-package #:cl-prolog.tests)

(deftest current-predicate-uses-logical-update-snapshot ()
  (let ((rulebase (make-rulebase))
        (seen '()))
    (rulebase-insert-clause! rulebase (make-clause '(alpha first)))
    (rulebase-insert-clause! rulebase (make-clause '(alpha second)))
    (rulebase-insert-clause! rulebase (make-clause '(beta)))
    (map-prolog-solutions
     (lambda (solution)
       (push (solution-binding '?indicator solution) seen)
       (rulebase-insert-clause! rulebase (make-clause '(gamma added))))
     rulebase '(current_predicate ?indicator))
    (is-equal '((/ alpha 1) (/ beta 0))
              (remove-if-not
               (lambda (indicator)
                 (member (second indicator) '(alpha beta gamma)))
               (nreverse seen)))
    (assert-query rulebase (current_predicate (/ gamma 1)) :succeeds)))

(deftest current-predicate-validates-ground-indicators ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (current_predicate missing) :signals)
    (assert-query rulebase (current_predicate (/ missing atom)) :signals)
    (assert-query rulebase (current_predicate (/ ?name atom)) :signals)
    (assert-query rulebase (current_predicate (/ ?name 1 ?extra)) :signals)
    (assert-query rulebase (current_predicate (/ missing -1)) :signals)
    (assert-query rulebase (current_predicate (/ missing 1)) :fails)
    (assert-query rulebase (current_predicate (/ foreign-choice 1)) :succeeds)
    (assert-query rulebase (current_predicate (/ ?name 2)) :succeeds)
    (assert-query rulebase (current_predicate (/ = ?arity)) :succeeds)))

(deftest dynamic-database-enforces-static-procedure-permissions ()
  (let ((rulebase (make-rulebase
                   :clauses (list (make-clause '(fixed original))))))
    (dolist (goal '((asserta (fixed replacement))
                    (assertz (= left left))
                    (retract (fixed ?value))
                    (abolish (/ fixed 1))))
      (handler-case
          (progn
            (query-prolog rulebase goal)
            (error "Expected a permission error for ~S" goal))
        (prolog-permission-error (condition)
          (let ((formal (second (prolog-exception-term condition))))
            (is-equal "PERMISSION_ERROR" (symbol-name (first formal)))
            (is-equal "MODIFY" (symbol-name (second formal)))
            (is-equal "STATIC_PROCEDURE" (symbol-name (third formal)))))))))

(deftest rejected-static-assertion-preserves-predicate-property ()
  (let ((rulebase (make-rulebase
                   :clauses (list (make-clause '(fixed original))))))
    (signals-error (query-prolog rulebase '(assertz (fixed replacement))))
    (assert-query rulebase (fixed original) :succeeds)
    (assert-query rulebase (fixed replacement) :fails)
    (assert-query rulebase (predicate_property (fixed ?value) static)
                  :succeeds)))

(deftest dynamic-database-validates-callable-arguments ()
  (let ((rulebase (make-rulebase)))
    (dolist (goal '((retract ?clause)
                    (retractall ?clause)
                    (clause ?head ?body)))
      (handler-case
          (progn
            (query-prolog rulebase goal)
            (error "Expected an instantiation error for ~S" goal))
        (prolog-instantiation-error (condition)
          (is-equal "INSTANTIATION_ERROR"
                    (symbol-name (second (prolog-exception-term condition)))))))
    (dolist (goal '((retract 42)
                    (retractall 42)
                    (clause 42 ?body)))
      (handler-case
          (progn
            (query-prolog rulebase goal)
            (error "Expected a callable type error for ~S" goal))
        (prolog-type-error (condition)
          (let ((formal (second (prolog-exception-term condition))))
            (is-equal "TYPE_ERROR" (symbol-name (first formal)))
            (is-equal "CALLABLE" (symbol-name (second formal)))))))))

(deftest clause-rejects-static-procedure-access ()
  (let ((rulebase (make-rulebase
                   :clauses (list (make-clause '(fixed original))))))
    (handler-case
        (progn
          (query-prolog rulebase '(clause (fixed ?value) ?body))
          (error "Expected a static access permission error"))
      (prolog-permission-error (condition)
        (let ((formal (second (prolog-exception-term condition))))
          (is-equal '("PERMISSION_ERROR" "ACCESS" "PRIVATE_PROCEDURE")
                    (mapcar #'symbol-name (subseq formal 0 3)))
          (is-equal '(/ fixed 1) (fourth formal)))))))

(deftest abolish-validates-predicate-indicators ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (abolish ?indicator) :signals)
    (dolist (goal '((abolish fixed)
                    (abolish (/ fixed one))))
      (assert-query rulebase goal :signals))
    (assert-query rulebase (abolish (/ fixed -1)) :signals)))

(deftest proper-list-p-rejects-circular-lists ()
  (let ((circular (list 'value)))
    (setf (cdr circular) circular)
    (is (not (cl-prolog::%proper-list-p circular)))))

(deftest abolish-removes-the-dynamic-declaration ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (assertz (temporary first)) :succeeds)
    (assert-query rulebase (abolish (/ temporary 1)) :succeeds)
    (is (null (cl-prolog::%rulebase-predicate-property
               rulebase 'temporary 1)))
    (assert-query rulebase (assertz (temporary second)) :succeeds)
    (assert-query rulebase (temporary ?value) => (((?value . second))))))

(deftest abolish-removes-table-declarations ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (assertz (tabled-dynamic value)) :succeeds)
    (cl-prolog::%add-rulebase-table-declaration!
     rulebase 'tabled-dynamic 1 :runtime)
    (is (cl-prolog::%rulebase-tabled-p rulebase 'tabled-dynamic 1))
    (assert-query rulebase (abolish (/ tabled-dynamic 1)) :succeeds)
    (is (not (cl-prolog::%rulebase-tabled-p
              rulebase 'tabled-dynamic 1)))))

(deftest current-predicate-includes-empty-dynamic-procedures ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase (assertz (empty-after-retract value)) :succeeds)
    (assert-query rulebase (retractall (empty-after-retract ?value)) :succeeds)
    (assert-query rulebase
                  (current_predicate (/ empty-after-retract 1)) :succeeds)))

(deftest predicate-call-keeps-logical-update-snapshot ()
  (let ((rulebase (make-rulebase))
        (seen '()))
    (assert-query rulebase (assertz (item first)) :succeeds)
    (assert-query rulebase (assertz (item second)) :succeeds)
    (map-prolog-solutions
     (lambda (solution)
       (push (solution-binding '?value solution) seen)
       (query-prolog rulebase '(retractall (item ?discarded))))
     rulebase '(item ?value))
    (is-equal '(first second) (nreverse seen))
    (assert-query rulebase (item ?value) :fails)))
