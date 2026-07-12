(in-package #:cl-prolog)

(define-rulebase *family*
  ((parent tom bob))
  ((parent bob alice))
  ((ancestor ?x ?y) (parent ?x ?y))
  ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y)))

(format t "~&quick-start ancestor(tom, ?who) => ~S~%"
        (query-prolog *family* '(ancestor tom ?who)))
