;;;; Canonical Prolog term writer tests.

(in-package #:cl-prolog.tests)

(deftest prolog-term-writer-atoms-variables-and-numbers ()
  (is-equal "simple" (prolog-term-string 'cl-prolog::simple))
  (is-equal "'Mary Jane'"
            (prolog-term-string 'cl-prolog::|Mary Jane|))
  (is-equal "'can''t'"
            (prolog-term-string 'cl-prolog::|can't|))
  (is-equal "X" (prolog-term-string 'cl-prolog::?X))
  (is-equal "42" (prolog-term-string 42))
  (is-equal 1.5d0
            (read-prolog-term (prolog-term-string 1.5d0))))

(deftest prolog-term-writer-lists-and-compounds ()
  (is-equal "pair(a,2)"
            (prolog-term-string
             '(cl-prolog::pair cl-prolog::a 2)))
  (is-equal "[1,2,3]" (prolog-term-string '(1 2 3)))
  (is-equal "[1,2|TAIL]"
            (prolog-term-string '(1 2 . cl-prolog::?TAIL)))
  (is-equal '() (read-prolog-term (prolog-term-string '())))
  (is-equal '(cl-prolog::pair cl-prolog::a 2)
            (read-prolog-term
             (prolog-term-string
              '(cl-prolog::pair cl-prolog::a 2)))))

(deftest prolog-term-writer-preserves-operator-precedence ()
  (dolist (source '("1 + 2 * 3"
                    "(1 + 2) * 3"
                    "8 - (3 - 1)"
                    "2 ^ 3 ^ 4"
                    "\\+ (X = 1)"
                    "p, q ; r"
                    "p -> q ; r"
                    "p *-> q ; r"
                    "pair(1 + 2, 3 * 4)"))
    (let* ((term (read-prolog-term source))
           (rendered (prolog-term-string term)))
      (is-equal term (read-prolog-term rendered)))))

(deftest prolog-term-writer-stream-api ()
  (let ((term '(cl-prolog::f cl-prolog::a)))
    (is (eq term
            (write-prolog-term term (make-broadcast-stream)))))
  (signals-error (prolog-term-string #\x)))
