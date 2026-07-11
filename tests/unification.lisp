;;;; Unification protocol tests.

(in-package #:fx.prolog.tests)

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
