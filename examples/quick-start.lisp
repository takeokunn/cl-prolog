(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "../scripts/bootstrap.lisp"
                         (or *load-truename* *load-pathname*))))

(unless (find-package "CL-PROLOG")
  (cl-prolog.bootstrap:load-core-sources))

(in-package #:cl-prolog)

(define-rulebase *family*
  ((parent tom bob))
  ((parent bob alice))
  ((ancestor ?x ?y) (parent ?x ?y))
  ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y)))

(format t "~&quick-start ancestor(tom, ?who) => ~S~%"
        (query-prolog *family* '(ancestor tom ?who)))
