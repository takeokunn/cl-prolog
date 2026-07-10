(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(unless (find-package "FX.PROLOG")
  (require :asdf)
  (asdf:load-asd (truename (merge-pathnames "../cl-prolog.asd"
                                           (or *load-truename* *load-pathname*))))
  (asdf:load-system :cl-prolog))

(in-package #:fx.prolog)

(format t "~&append(?l ?r (a b c)) => ~S~%"
        (query-prolog *global-rulebase* '(append ?l ?r (a b c))))
(format t "reverse(?xs (c b a)) => ~S~%"
        (query-prolog *global-rulebase* '(reverse ?xs (c b a))))
(format t "length(?xs 3) => ~S~%"
        (query-prolog-first *global-rulebase* '(length ?xs 3)))
(format t "member(?x (a b c)) => ~S~%"
        (query-prolog *global-rulebase* '(member ?x (a b c))))
