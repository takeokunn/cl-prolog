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
            (read-prolog-term (prolog-term-string 1.5d0)))
  (is-equal "!" (prolog-term-string 'cl-prolog::|!|)))

(deftest prolog-term-writer-handles-malformed-numbervars-and-fx-prefixes ()
  (let ((term (list (cl-prolog::%prolog-atom-symbol "$VAR" :preserve-case t)
                     0 'cl-prolog::extra)))
    (is-equal term (read-prolog-term (prolog-term-string term))))
  (let ((term (read-prolog-term ":- foo.")))
    (is-equal term (read-prolog-term (prolog-term-string term)))))

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

(cl-weave:it-property
    "reading a ground term's canonical printed form reproduces an equal term"
    ((term (cl-weave:gen-recursive
            (cl-weave:gen-one-of
             (cl-weave:gen-integer :min -1000 :max 1000)
             (cl-weave:gen-member
              '(cl-prolog::a cl-prolog::b cl-prolog::foo cl-prolog::bar
                cl-prolog::baz)))
            (lambda (self) (cl-weave:gen-list self :min-length 1 :max-length 3))
            :max-depth 3)))
  (cl-weave:expect-has-assertions)
  (cl-weave:expect (read-prolog-term (prolog-term-string term)) :to-equal term))

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

(deftest prolog-term-writer-renders-precedence-exactly ()
  "The round-trip test above only checks that rendering re-parses to the same
term; these inline snapshots pin the exact rendered layout (operator
spacing, minimal parenthesization) so a formatting regression is visible as
a diff, not just a semantic pass/fail."
  (cl-weave:expect (prolog-term-string (read-prolog-term "1 + 2 * 3"))
                   :to-match-inline-snapshot "\"1 + 2 * 3\"")
  (cl-weave:expect (prolog-term-string (read-prolog-term "(1 + 2) * 3"))
                   :to-match-inline-snapshot "\"(1 + 2) * 3\"")
  (cl-weave:expect (prolog-term-string (read-prolog-term "8 - (3 - 1)"))
                   :to-match-inline-snapshot "\"8 - (3 - 1)\"")
  (cl-weave:expect (prolog-term-string (read-prolog-term "2 ^ 3 ^ 4"))
                   :to-match-inline-snapshot "\"2 ^ 3 ^ 4\"")
  (cl-weave:expect (prolog-term-string (read-prolog-term "\\+ (X = 1)"))
                   :to-match-inline-snapshot "\"\\\\+ X = 1\"")
  (cl-weave:expect (prolog-term-string (read-prolog-term "p, q ; r"))
                   :to-match-inline-snapshot "\"p , q ; r\"")
  (cl-weave:expect (prolog-term-string (read-prolog-term "p -> q ; r"))
                   :to-match-inline-snapshot "\"p -> q ; r\"")
  (cl-weave:expect (prolog-term-string (read-prolog-term "pair(1 + 2, 3 * 4)"))
                   :to-match-inline-snapshot "\"pair(1 + 2,3 * 4)\""))

(deftest prolog-term-writer-stream-api ()
  (let ((term '(cl-prolog::f cl-prolog::a)))
    (is (eq term
            (write-prolog-term term (make-broadcast-stream)))))
  (signals-error (prolog-term-string #\x)))
