(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "../scripts/bootstrap.lisp"
                         (or *load-truename* *load-pathname*))))

(unless (find-package "FX.PROLOG")
  (cl-prolog.bootstrap:load-core-sources))

(in-package #:fx.prolog)

(format t "~&append(?l ?r (a b c)) => ~S~%"
        (query-prolog *global-rulebase* '(append ?l ?r (a b c))))
(format t "reverse(?xs (c b a)) => ~S~%"
        (query-prolog *global-rulebase* '(reverse ?xs (c b a))))
(format t "length(?xs 3) => ~S~%"
        (query-prolog-first *global-rulebase* '(length ?xs 3)))
(format t "member(?x (a b c)) => ~S~%"
        (query-prolog *global-rulebase* '(member ?x (a b c))))
