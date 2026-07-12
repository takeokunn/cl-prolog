;;;; Atom, character, and numeric text builtin contract.

(in-package #:cl-prolog.tests)

(defmacro deftest-bidirectional-queries (name (rulebase-form) &body cases)
  "Define NAME from paired query cases that share a predicate shape.

Each case is (PREDICATE FORWARD-INPUT FORWARD-OUTPUT FORWARD-EXPECTED
                     REVERSE-INPUT REVERSE-OUTPUT REVERSE-EXPECTED).
The macro expands each case into two DEFTEST-QUERIES specs."
  `(deftest-queries ,name (,rulebase-form)
     ,@(loop for case in cases
             append (destructuring-bind (predicate forward-input forward-output
                                          forward-expected reverse-input
                                          reverse-output reverse-expected)
                        case
                      `(((,predicate ,forward-input ,forward-output)
                         => ,forward-expected)
                        ((,predicate ,reverse-input ,reverse-output)
                         => ,reverse-expected))))))

(deftest-queries atom-builtins ((make-rulebase))
  ((cl-prolog::atom_length cl-prolog::hello ?length) => (((?length . 5))))
  ((cl-prolog::atom_length cl-prolog::hello 5) :succeeds)
  ((cl-prolog::atom_length cl-prolog::hello 4) :fails)
  ((cl-prolog::atom_concat cl-prolog::hello cl-prolog::world ?whole)
   => (((?whole . cl-prolog::helloworld))))
  ((cl-prolog::atom_concat ?left ?right cl-prolog::abc)
   => (((?left . cl-prolog::||) (?right . cl-prolog::abc))
       ((?left . cl-prolog::a) (?right . cl-prolog::bc))
       ((?left . cl-prolog::ab) (?right . cl-prolog::c))
       ((?left . cl-prolog::abc) (?right . cl-prolog::||))))
  ((cl-prolog::atom_concat cl-prolog::a ?right cl-prolog::abc)
   => (((?right . cl-prolog::bc))))
  ((cl-prolog::sub_atom cl-prolog::abc ?before ?length ?after ?sub)
   :succeeds :limit 10)
  ((cl-prolog::sub_atom cl-prolog::abc 1 1 ?after ?sub)
   => (((?after . 1) (?sub . cl-prolog::b)))))

(deftest-bidirectional-queries atom-builtins-text ((make-rulebase))
  (cl-prolog::atom_chars cl-prolog::abc ?chars
   (((?chars cl-prolog::a cl-prolog::b cl-prolog::c)))
   ?atom (cl-prolog::a cl-prolog::b cl-prolog::c)
   (((?atom . cl-prolog::abc))))
  (cl-prolog::atom_codes cl-prolog::abc ?codes
   (((?codes 65 66 67)))
   ?atom (65 66 67)
   (((?atom . cl-prolog::abc))))
  (cl-prolog::char_code cl-prolog::a ?code
   (((?code . 65)))
   ?character 65
   (((?character . cl-prolog::a))))
  (cl-prolog::number_chars 42 ?chars
   (((?chars cl-prolog::|4| cl-prolog::|2|)))
   ?number (cl-prolog::|4| cl-prolog::|2|)
   (((?number . 42))))
  (cl-prolog::number_codes -17 ?codes
   (((?codes 45 49 55)))
   ?number (45 49 55)
   (((?number . -17))))
  (cl-prolog::atom_number cl-prolog::|42| ?number
   (((?number . 42)))
   ?atom 42
   (((?atom . cl-prolog::|42|)))))

(deftest-queries atom-number-builtins ((make-rulebase))
  ((cl-prolog::atom_number cl-prolog::|-0.125| ?number)
   => (((?number . -0.125d0))))
  ((cl-prolog::atom_number cl-prolog::|42| 42) :succeeds)
  ((cl-prolog::atom_number cl-prolog::|42| 43) :fails)
  ((cl-prolog::atom_number cl-prolog::bad ?number) :fails))

(defun parse-number-codes (codes)
  (let ((solutions (query-prolog
                    (make-rulebase)
                    `(cl-prolog::number_codes ?number ,codes))))
    (cdr (assoc '?number (first solutions)))))

(defun number-codes-error-type (codes)
  (handler-case
      (progn (parse-number-codes codes) nil)
    (prolog-runtime-error (condition)
      (type-of condition))))

(defun round-trip-number-codes (number)
  (let* ((encoded (query-prolog
                   (make-rulebase)
                   `(cl-prolog::number_codes ,number ?codes)))
         (codes (cdr (assoc '?codes (first encoded)))))
    (parse-number-codes codes)))

(defun number-output-error-type (number)
  (handler-case
      (progn
        (query-prolog (make-rulebase)
                      `(cl-prolog::number_codes ,number ?codes))
        nil)
    (prolog-runtime-error (condition)
      (type-of condition))))

(deftest-table atom-number-text-grammar ()
  (:equal 0 (parse-number-codes '(48)))
  (:equal 17 (parse-number-codes '(43 49 55)))
  (:equal -17 (parse-number-codes '(45 49 55)))
  (:equal 12.5d0 (parse-number-codes '(49 50 46 53)))
  (:equal 1250.0d0 (parse-number-codes '(49 46 50 53 69 43 51)))
  (:equal 0.0125d0 (parse-number-codes '(49 46 50 53 101 45 50)))
  (:equal 'prolog-domain-error (number-codes-error-type '(49 47 50)))
  (:equal 'prolog-domain-error (number-codes-error-type '(49 46)))
  (:equal 'prolog-domain-error (number-codes-error-type '(46 53)))
  (:equal 'prolog-domain-error (number-codes-error-type '(49 101)))
  (:equal 'prolog-domain-error (number-codes-error-type '(49 50 120))))

(deftest-table atom-number-text-round-trips ()
  (:equal 42 (round-trip-number-codes 42))
  (:equal 12.5d0 (round-trip-number-codes 12.5d0))
  (:equal -0.125d0 (round-trip-number-codes -0.125d0))
  (:equal 1.25d20 (round-trip-number-codes 1.25d20))
  (:equal 1.25d-20 (round-trip-number-codes 1.25d-20))
  (:equal 'prolog-domain-error
          (number-output-error-type 1/2)))

(deftest atom-list-conversion-rejects-cycles ()
  (let ((cycle (list (cl-prolog::%text-atom "a"))))
    (setf (cdr cycle) cycle)
    (handler-case
        (progn
          (cl-prolog::%character-list-text
           cycle nil (cl-prolog::%iso-atom "ATOM_CHARS"))
          (error "Expected cyclic list rejection"))
      (prolog-type-error (condition)
        (declare (ignore condition))
        (is t "Cyclic lists must raise a Prolog type error")))))

(deftest-table atom-builtins-report-iso-errors ()
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (builtin-error-summary '(cl-prolog::atom_length ?atom ?length)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "ATOM" 1))
          (builtin-error-summary '(cl-prolog::atom_length 1 ?length)))
  (:equal '(prolog-domain-error ("DOMAIN_ERROR" "NOT_LESS_THAN_ZERO" -1))
          (builtin-error-summary '(cl-prolog::atom_length atom -1)))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (builtin-error-summary '(cl-prolog::atom_concat ?a right ?whole)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "ATOM" 3))
          (builtin-error-summary '(cl-prolog::atom_concat 3 right whole)))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (builtin-error-summary '(cl-prolog::sub_atom ?atom 0 1 ?after ?sub)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "INTEGER" 1.5))
          (builtin-error-summary '(cl-prolog::sub_atom abc 1.5 1 ?after ?sub)))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (builtin-error-summary '(cl-prolog::atom_chars ?atom ?chars)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "CHARACTER" "AB"))
          (builtin-error-summary '(cl-prolog::atom_chars ?atom (ab))))
  (:equal '(prolog-type-error ("TYPE_ERROR" "INTEGER" "ATOM"))
          (builtin-error-summary '(cl-prolog::atom_codes ?atom (atom))))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (builtin-error-summary '(cl-prolog::char_code ?character ?code)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "CHARACTER" "AB"))
          (builtin-error-summary '(cl-prolog::char_code ab ?code)))
  (:equal '(prolog-domain-error ("DOMAIN_ERROR" "CHARACTER_CODE" -1))
          (builtin-error-summary '(cl-prolog::char_code ?character -1)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "NUMBER" "ATOM"))
          (builtin-error-summary '(cl-prolog::number_chars atom ?chars)))
  (:equal '(prolog-domain-error ("DOMAIN_ERROR" "NUMBER_TEXT" "bad"))
          (builtin-error-summary '(cl-prolog::number_codes ?number (98 97 100))))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (builtin-error-summary '(cl-prolog::atom_number ?atom ?number)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "ATOM" 1))
          (builtin-error-summary '(cl-prolog::atom_number 1 ?number)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "NUMBER" "ATOM"))
          (builtin-error-summary '(cl-prolog::atom_number ?atom atom)))
  (:equal '(prolog-domain-error ("DOMAIN_ERROR" "PROLOG_NUMBER" 1/2))
          (builtin-error-summary '(cl-prolog::atom_number ?atom 1/2))))
