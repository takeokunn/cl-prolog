(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "../scripts/bootstrap.lisp"
                         (or *load-truename* *load-pathname*))))

(unless (find-package "FX.PROLOG")
  (cl-prolog.bootstrap:load-core-sources))

(in-package #:fx.prolog)

(let ((rulebase (make-rulebase)))
  (format t "~&append(?l ?r (a b c)) => ~S~%"
          (query-prolog rulebase '(append ?l ?r (a b c))))
  (format t "reverse(?xs (c b a)) => ~S~%"
          (query-prolog rulebase '(reverse ?xs (c b a))))
  (format t "length(?xs 3) => ~S~%"
          (query-prolog-first rulebase '(length ?xs 3)))
  (format t "member(?x (a b c)) => ~S~%"
          (query-prolog rulebase '(member ?x (a b c)))))
