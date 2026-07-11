;;;; Shared fixture data and rulebases.

(in-package #:fx.prolog.tests)

(defvar *recorded-colors* '())

(defmethod predicate-true-p ((predicate (eql 'record-color)) args bindings)
  (push (logic-substitute (first args) bindings) *recorded-colors*)
  t)

(defmethod predicate-true-p ((predicate (eql 'accept-ok)) args bindings)
  (equal (mapcar (lambda (arg) (logic-substitute arg bindings)) args)
         '(:ok)))

(define-rulebase *macro-rulebase*
  ((parent tom bob))
  ((parent bob alice))
  ((ancestor ?x ?y) (parent ?x ?y))
  ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y)))

(defun make-family-rulebase ()
  (prolog
    ((parent tom bob))
    ((parent bob alice))
    ((parent alice eve))
    ((choice left))
    ((choice right))
    ((colored ?x) (parent ?x bob) (record-color ?x))
    ((accepted ?x) (accept-ok ?x))
    ((adult ?x) (parent ?x ?y) (:when (symbolp ?y)))
    ((ancestor ?x ?y) (parent ?x ?y))
    ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y))
    ((grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))))

(defmacro with-clean-global-rulebase (&body body)
  `(let ((*global-rulebase* (make-rulebase))
         (*recorded-colors* '()))
     ,@body))
