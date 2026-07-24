;;;; Shared fixture data and rulebases.

(in-package #:cl-prolog.tests)

(defvar *recorded-colors* '())

(define-foreign-predicate (record-color color) (rulebase environment depth emit)
  (push (logic-substitute color environment) *recorded-colors*)
  (funcall emit environment))

(define-foreign-predicate (accept-ok value) (rulebase environment depth emit)
  (when (eql (logic-substitute value environment) :ok)
    (funcall emit environment)))

(define-foreign-predicate (foreign-choice value) (rulebase environment depth emit)
  (dolist (choice '(left right))
    (multiple-value-bind (extended matched)
        (unify value choice environment)
      (when matched
        (funcall emit extended)))))

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
