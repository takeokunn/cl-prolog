;;;; Conventional Prolog source parser tests.

(in-package #:cl-prolog.tests)

(deftest prolog-term-parser ()
  (is-equal '(cl-prolog::person cl-prolog::|Mary Jane| 12 1.5d0)
            (read-prolog-term "person('Mary Jane', 12, 1.5)."))
  (is-equal '(cl-prolog::a cl-prolog::b cl-prolog::c)
            (read-prolog-term "[a,b,c]"))
  (is-equal '(cl-prolog::a cl-prolog::b . cl-prolog::?TAIL)
             (read-prolog-term "[a,b|Tail]"))
  (is-equal 1000.0d0 (read-prolog-term "1e3"))
  (is-equal 0.015d0 (read-prolog-term "1.5e-2"))
  (signals-error (read-prolog-term "[a|X,Y]"))
  (let ((term (read-prolog-term "pair(X, X, _, _)")))
    (is (eq (second term) (third term)))
    (is (not (eq (fourth term) (fifth term)))))
  (let ((nil-atom (read-prolog-term "nil"))
        (car-atom (read-prolog-term "car")))
    (is (not (null nil-atom)))
    (is (eq nil-atom (read-prolog-term "'NIL'")))
    (is (eq (symbol-package nil-atom)
            (find-package '#:cl-prolog.user-atoms)))
    (is (not (eq car-atom 'cl:car)))
    (is-equal '() (read-prolog-term "[]"))))

(deftest prolog-stream-term-reader-is-incremental ()
  (with-input-from-string
      (stream (format nil
                      "first(1.5, 'not.a.term'). % between terms~%second([a,b])."))
    (is-equal '(cl-prolog.user-atoms::first 1.5d0 cl-prolog::|not.a.term|)
              (read-prolog-term stream))
    (is-equal '(cl-prolog.user-atoms::second (cl-prolog::a cl-prolog::b))
              (read-prolog-term stream)))
  (with-input-from-string (stream "value % comment at end of file")
    (is-equal 'cl-prolog::value (read-prolog-term stream))))

(deftest prolog-clause-parser ()
  (let ((fact (read-prolog-clause "parent(tom, bob)."))
        (rule (read-prolog-clause "ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).")))
    (is-equal '(cl-prolog::parent cl-prolog::tom cl-prolog::bob) (clause-head fact))
    (is-equal '() (clause-body fact))
    (is-equal '(cl-prolog::ancestor cl-prolog::?X cl-prolog::?Y) (clause-head rule))
    (is-equal '((cl-prolog::parent cl-prolog::?X cl-prolog::?Z)
                (cl-prolog::ancestor cl-prolog::?Z cl-prolog::?Y))
              (clause-body rule)))
  (let ((rule (read-prolog-clause "ready :- true.")))
    (is-equal '(cl-prolog::ready) (clause-head rule))
    (is-equal '(cl-prolog::true) (clause-body rule))))

(deftest prolog-operator-parser ()
  (is-equal '(cl-prolog::or (cl-prolog::and cl-prolog::p cl-prolog::q) cl-prolog::r)
            (read-prolog-term "p, q ; r"))
  (is-equal '(cl-prolog::if-then-else cl-prolog::p cl-prolog::q cl-prolog::r)
            (read-prolog-term "p -> q ; r"))
  (is-equal '(cl-prolog::soft-if-then-else cl-prolog::p cl-prolog::q cl-prolog::r)
            (read-prolog-term "p *-> q ; r"))
  (is-equal '(not (= cl-prolog::?X 1)) (read-prolog-term "\\+ X = 1"))
  (is-equal '(is cl-prolog::?X (+ 1 (* 2 3))) (read-prolog-term "X is 1 + 2 * 3"))
  (signals-error (read-prolog-term "2 ** 3 ** 4"))
  (is-equal '(cl-prolog::^ 2 (cl-prolog::^ 3 4))
            (read-prolog-term "2 ^ 3 ^ 4"))
  (is-equal '(cl-prolog::rem (cl-prolog::div (cl-prolog::// 20 3) 2) 2)
            (read-prolog-term "20 // 3 div 2 rem 2"))
  (is-equal '(cl-prolog::mod 17 5) (read-prolog-term "17 mod 5"))
  (is-equal '(cl-prolog::@< cl-prolog::a cl-prolog::b)
            (read-prolog-term "a @< b"))
  (is-equal '(cl-prolog:|\\=| cl-prolog::?X cl-prolog::?Y)
            (read-prolog-term "X \\= Y"))
  (is-equal '(cl-prolog::=.. cl-prolog::?X
              (cl-prolog::foo cl-prolog::a cl-prolog::b))
             (read-prolog-term "X =.. [foo,a,b]"))
  (dolist (operator '("=" "\\=" "==" "\\==" "=:=" "=\\=" "=.."
                      "=<" ">=" "<" ">" "is"))
    (signals-error
     (read-prolog-term (format nil "X ~A Y ~A Z" operator operator))))
  (signals-error (read-prolog-term "X = Y < Z"))
  (signals-error (read-prolog-term "X = \\+ p"))
  (is-equal '(cl-prolog::pair (+ 1 2) (* 3 4))
            (read-prolog-term "pair(1 + 2, 3 * 4)")))

(deftest prolog-operator-table-drives-parser ()
  (dolist (case '(("1 + 2 * 3" (+ 1 (* 2 3)))
                  ("8 - 3 - 1" (- (- 8 3) 1))
                  ("2 ^ 3 ^ 4" (cl-prolog::^ 2 (cl-prolog::^ 3 4)))
                  ("\\+ X = 1" (not (= cl-prolog::?X 1)))))
    (destructuring-bind (source expected) case
      (is-equal expected (read-prolog-term source))))
  (dolist (definition
           (cl-prolog::%operator-table-current
            cl-prolog::*standard-operator-table*))
    (let ((lexeme (cl-prolog::%operator-lexeme definition)))
      (if (cl-prolog::%word-operator-lexeme-p lexeme)
          (is (member lexeme (cl-prolog::%standard-operator-lexemes t)
                      :test #'string=))
          (is (member lexeme (cl-prolog::%symbolic-token-lexemes)
                      :test #'string=))))))

(progn
(deftest operator-lexeme-generation-does-not-mutate-delimiters ()
  (dotimes (index 20)
    (declare (ignore index))
    (is-equal 8
              (length (cl-prolog::%compute-symbolic-token-lexemes
                       (cl-prolog::%make-operator-table '())))))
  (is-equal 3
            (length (parse-prolog
                     "fact(a). rule(X) :- fact(X). ?- rule(X)."))))
(deftest quoted-question-atoms-use-distinct-atom-namespace ()
  (let* ((atom (read-prolog-term "'?x'."))
         (printed (prolog-term-string atom))
         (round-trip
           (read-prolog-term (concatenate 'string printed "."))))
    (is (eq (find-package '#:cl-prolog.user-atoms)
            (symbol-package atom)))
    (is (not (logic-var-p atom)))
    (is (not (eq atom 'cl-prolog::?x)))
    (is-equal "'?x'" printed)
    (is (eq atom round-trip)))
  (let* ((rulebase (consult-prolog "p('?x')."))
         (atom-query (read-prolog-term "p('?x')."))
         (other-query (read-prolog-term "p(a).")))
    (is-equal '(nil) (query-prolog rulebase atom-query))
    (is (prolog-succeeds-p rulebase atom-query))
    (is (not (prolog-succeeds-p rulebase other-query))))))

(progn
(deftest prolog-source-parser-and-consult ()
  (let* ((source (format nil "% family~% parent(tom,bob). /* rule */~% child(X) :- parent(tom,X).~% ?- child(X)."))
         (forms (parse-prolog source)))
    (is-equal 3 (length forms))
    (is (clause-p (first forms)))
    (is (clause-p (second forms)))
    (is-equal '(cl-prolog::child cl-prolog::?X) (third forms)))
  (let ((rulebase (consult-prolog "edge(a,b). edge(b,c).")))
    (assert-query rulebase (cl-prolog::edge cl-prolog::a ?x)
                  => (((?x . cl-prolog::b)))))
  (signals-error (consult-prolog "?- true."))
  (let ((rulebase (make-rulebase)))
    (signals-error (consult-prolog "kept. ?- kept." rulebase))
    (is-equal '() (rulebase-visible-clauses rulebase))))

(defun %parser-resource-condition (thunk)
  (handler-case
      (progn
        (funcall thunk)
        nil)
    (prolog-parser-resource-error (condition)
      condition)))

(defun %prolog-parse-error-p (thunk)
  (handler-case
      (progn
        (funcall thunk)
        nil)
    (cl-prolog::prolog-parse-error ()
      t)))

(deftest prolog-parser-enforces-source-and-token-limits ()
  (let ((*max-prolog-source-characters* 1))
    (is-equal (quote cl-prolog::a)
              (read-prolog-term "a")))
  (let* ((*max-prolog-source-characters* 1)
         (condition
           (%parser-resource-condition
            (lambda () (read-prolog-term "ab")))))
    (is condition)
    (is-equal "SOURCE_CHARACTERS"
              (prolog-parser-resource-error-resource condition))
    (is-equal 1 (prolog-parser-resource-error-limit condition))
    (is-equal 2 (prolog-parser-resource-error-observed condition))
    (is-equal 2 (prolog-parser-resource-error-position condition)))
  (with-input-from-string (stream "a.")
    (let ((*max-prolog-source-characters* 2))
      (is-equal (quote cl-prolog::a)
                (read-prolog-term stream))))
  (with-input-from-string (stream "a.")
    (let* ((*max-prolog-source-characters* 1)
           (condition
             (%parser-resource-condition
              (lambda () (read-prolog-term stream)))))
      (is condition)
      (is-equal "SOURCE_CHARACTERS"
                (prolog-parser-resource-error-resource condition))
      (is-equal 2 (prolog-parser-resource-error-position condition))))
  (let ((*max-prolog-tokens* 1))
    (is-equal (quote cl-prolog::a)
              (read-prolog-term "a")))
  (let* ((*max-prolog-tokens* 0)
         (condition
           (%parser-resource-condition
            (lambda () (read-prolog-term "a")))))
    (is condition)
    (is-equal "TOKEN_COUNT"
              (prolog-parser-resource-error-resource condition))
    (is-equal 0 (prolog-parser-resource-error-limit condition))
    (is-equal 1 (prolog-parser-resource-error-observed condition))
    (is-equal 0 (prolog-parser-resource-error-position condition))))

(deftest prolog-parser-enforces-lexeme-limits ()
  (let ((*max-prolog-identifier-length* 3))
    (is-equal (quote cl-prolog::abc)
              (read-prolog-term "abc")))
  (let* ((*max-prolog-identifier-length* 2)
         (condition
           (%parser-resource-condition
            (lambda () (read-prolog-term "abc")))))
    (is condition)
    (is-equal "IDENTIFIER_LENGTH"
              (prolog-parser-resource-error-resource condition))
    (is-equal 3 (prolog-parser-resource-error-observed condition))
    (is-equal 2 (prolog-parser-resource-error-position condition)))
  (let ((source (format nil "~Cabc~C" (code-char 39) (code-char 39))))
    (let ((*max-prolog-quoted-lexeme-length* 3))
      (let ((term (read-prolog-term source)))
        (is-equal "abc" (symbol-name term))
        (is (eq (find-package "CL-PROLOG")
                (symbol-package term)))))
    (let* ((*max-prolog-quoted-lexeme-length* 2)
           (condition
             (%parser-resource-condition
              (lambda () (read-prolog-term source)))))
      (is condition)
      (is-equal "QUOTED_LEXEME_LENGTH"
                (prolog-parser-resource-error-resource condition))
      (is-equal 3 (prolog-parser-resource-error-observed condition))))
  (let ((*max-prolog-numeric-lexeme-length* 3))
    (is-equal 123 (read-prolog-term "123")))
  (let* ((*max-prolog-numeric-lexeme-length* 2)
         (condition
           (%parser-resource-condition
            (lambda () (read-prolog-term "123")))))
    (is condition)
    (is-equal "NUMERIC_LEXEME_LENGTH"
              (prolog-parser-resource-error-resource condition))
    (is-equal 3 (prolog-parser-resource-error-observed condition))
    (is-equal 2 (prolog-parser-resource-error-position condition))))

(deftest prolog-parser-enforces-structural-limits ()
  (let ((*max-prolog-delimiter-depth* 1))
    (is-equal (quote cl-prolog::a)
              (read-prolog-term "(a)")))
  (let* ((*max-prolog-delimiter-depth* 1)
         (condition
           (%parser-resource-condition
            (lambda () (read-prolog-term "((a))")))))
    (is condition)
    (is-equal "DELIMITER_DEPTH"
              (prolog-parser-resource-error-resource condition))
    (is-equal 2 (prolog-parser-resource-error-observed condition)))
  (let ((*max-prolog-parser-depth* 1))
    (is-equal (quote cl-prolog::a)
              (read-prolog-term "a")))
  (let* ((*max-prolog-delimiter-depth* nil)
         (*max-prolog-parser-depth* 1)
         (condition
           (%parser-resource-condition
            (lambda () (read-prolog-term "(a)")))))
    (is condition)
    (is-equal "PARSER_DEPTH"
              (prolog-parser-resource-error-resource condition))
    (is-equal 2 (prolog-parser-resource-error-observed condition)))
  (dolist (source (list "(a]" ")"))
    (is (%prolog-parse-error-p
         (lambda () (read-prolog-term source)))))
  (dolist (source (list "(a]" ")"))
    (with-input-from-string (stream source)
      (is (%prolog-parse-error-p
           (lambda () (read-prolog-term stream)))))))

(deftest prolog-parser-bounds-interning-and-operator-caches ()
  (let ((cl-prolog::*parser-interned-symbols*
          (make-hash-table :test (function equal)))
        (*max-prolog-interned-symbols* 1))
    (read-prolog-term "security_parser_atom_7f41a")
    (let ((condition
            (%parser-resource-condition
             (lambda ()
               (read-prolog-term "security_parser_atom_7f41b")))))
      (is condition)
      (is-equal "INTERNED_SYMBOLS"
                (prolog-parser-resource-error-resource condition))))
  (let ((cl-prolog::*parser-interned-symbols*
          (make-hash-table :test (function equal)))
        (*max-prolog-interned-symbols* 1))
    (read-prolog-term "SecurityParserVar7f41a")
    (let ((condition
            (%parser-resource-condition
             (lambda ()
               (read-prolog-term "SecurityParserVar7f41b")))))
      (is condition)
      (is-equal "INTERNED_SYMBOLS"
                (prolog-parser-resource-error-resource condition))))
  (let ((name "security_unknown_operator_7f41a"))
    (is (null (nth-value
               1
               (find-symbol (string-upcase name)
                            (find-package "CL-PROLOG")))))
    (is (null
         (cl-prolog::%operator-definition-for-token
          (cl-prolog::%parser #() cl-prolog::*standard-operator-table*)
          (cl-prolog::%token :operator name 0)
          (list :xfx))))
    (is (null (nth-value
               1
               (find-symbol (string-upcase name)
                            (find-package "CL-PROLOG"))))))
  (let ((cl-prolog::*operator-lexeme-cache*
          (make-hash-table :test (function eq))))
    (dotimes (index 20)
      (declare (ignore index))
      (cl-prolog::%operator-table-lexemes
       (cl-prolog::%make-operator-table (list))))
    (is-equal 0
              (hash-table-count cl-prolog::*operator-lexeme-cache*))
    (cl-prolog::%operator-table-lexemes
     cl-prolog::*standard-operator-table*)
    (is-equal 1
              (hash-table-count cl-prolog::*operator-lexeme-cache*)))))
