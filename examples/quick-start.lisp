(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(unless (find-package "FX.PROLOG")
  (require :asdf)
  (asdf:load-asd (truename (merge-pathnames "../cl-prolog.asd"
                                           (or *load-truename* *load-pathname*))))
  (asdf:load-system :cl-prolog))

(in-package #:fx.prolog)

(define-rulebase *family*
  ((parent tom bob))
  ((parent bob alice))
  ((ancestor ?x ?y) (parent ?x ?y))
  ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y)))

(format t "~&quick-start ancestor(tom, ?who) => ~S~%"
        (query-prolog *family* '(ancestor tom ?who)))
