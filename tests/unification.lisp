;;;; Unification protocol tests.

(in-package #:cl-prolog.tests)

(deftest-unification unification-protocol
  (:substitute (pair ?x ?y) (pair left right)
               :expected (pair left right))
  (:fails (tag a) (tag b))
  (:not-ok ?x (wrap ?x))
  (:not-ok (wrap ?x) ?x)
  (:ok "same" "same")
  (:ok 7 7)
  (:not-ok 7 8)
  (:substitute (:outer (:inner . ?side) ?side)
               (:outer (:inner . :buy) ?other)
               :expected (:outer (:inner . :buy) :buy))
  (:substitute ?x ?y
               :initial-env ((?y . done))
               :expected done))

(deftest cyclic-unification-and-substitution-terminate ()
  (let ((left (cons 'node nil))
        (right (cons 'node nil))
        (different (cons 'other nil)))
    (setf (cdr left) left
          (cdr right) right
          (cdr different) different)
    (is (nth-value 1 (unify left right)))
    (is (not (nth-value 1 (unify left different))))
    (let ((copy (logic-substitute left nil)))
      (is (not (eq copy left)))
      (is (eq copy (cdr copy))))))

(deftest cyclic-substitution-preserves-sharing-and-variables ()
  (let* ((variable (fresh-logic-variable "?CYCLE"))
         (shared (cons variable nil))
         (root (cons shared shared)))
    (setf (cdr shared) shared)
    (is (cl-prolog::%term-has-variables-p root))
    (let ((copy (logic-substitute root nil)))
      (is (eq (car copy) (cdr copy)))
      (is (eq (car copy) (cdr (car copy))))
      (is (eq variable (car (car copy)))))))

(deftest cyclic-variable-scan-and-query-collection-terminate ()
  (let ((ground-cycle (cons 'ground nil))
        (query-cycle (cons '?x nil)))
    (setf (cdr ground-cycle) ground-cycle
          (cdr query-cycle) query-cycle)
    (is (not (cl-prolog::%term-has-variables-p ground-cycle)))
    (is-equal '(?x) (cl-prolog::%collect-query-variables query-cycle))))
