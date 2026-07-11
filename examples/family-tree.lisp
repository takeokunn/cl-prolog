(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "../scripts/bootstrap.lisp"
                         (or *load-truename* *load-pathname*))))

(unless (find-package "CL-PROLOG")
  (cl-prolog.bootstrap:load-core-sources))

(in-package #:cl-prolog)

(define-rulebase *family*
  ((parent tom bob))
  ((parent bob alice))
  ((parent alice eve))
  ((ancestor ?x ?y) (parent ?x ?y))
  ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y)))

(format t "~&ancestor(tom, ?who) => ~S~%"
        (query-prolog *family* '(ancestor tom ?who)))
(format t "first ancestor(tom, ?who) => ~S~%"
        (query-prolog-first *family* '(ancestor tom ?who)))
(format t "parent(tom, bob) succeeds => ~S~%"
        (prolog-succeeds-p *family* '(parent tom bob)))
