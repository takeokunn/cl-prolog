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
                         :ordered ,forward-expected)
                        ((,predicate ,reverse-input ,reverse-output)
                         :ordered ,reverse-expected))))))

(deftest-queries atom-builtins ((make-rulebase))
  ((cl-prolog::atom_length cl-prolog::hello ?length) :ordered (((?length . 5))))
  ((cl-prolog::atom_length cl-prolog::hello 5) :succeeds)
  ((cl-prolog::atom_length cl-prolog::hello 4) :fails)
  ((cl-prolog::atom_concat cl-prolog::hello cl-prolog::world ?whole)
   :ordered (((?whole . cl-prolog::helloworld))))
  ((cl-prolog::atom_concat ?left ?right cl-prolog::abc)
   :ordered (((?left . cl-prolog::||) (?right . cl-prolog::abc))
       ((?left . cl-prolog::a) (?right . cl-prolog::bc))
       ((?left . cl-prolog::ab) (?right . cl-prolog::c))
       ((?left . cl-prolog::abc) (?right . cl-prolog::||))))
  ((cl-prolog::atom_concat cl-prolog::a ?right cl-prolog::abc)
   :ordered (((?right . cl-prolog::bc))))
  ((cl-prolog::atom_concat ?left cl-prolog::bc cl-prolog::abc)
   :ordered (((?left . cl-prolog::a))))
  ((cl-prolog::atom_concat ?left cl-prolog::bcdef cl-prolog::abc)
   :fails)
  ((cl-prolog::atom_concat cl-prolog::left ?right ?whole)
   :signals)
  ((cl-prolog::sub_atom cl-prolog::abc ?before ?length ?after ?sub)
   :succeeds)
  ((cl-prolog::sub_atom cl-prolog::abc 1 1 ?after ?sub)
   :ordered (((?after . 1) (?sub . cl-prolog::b))))
  ((cl-prolog::sub_atom cl-prolog::abc ?before 2 ?after ?sub)
   :ordered (((?before . 0) (?after . 1) (?sub . cl-prolog::ab))
       ((?before . 1) (?after . 0) (?sub . cl-prolog::bc))))
  ((cl-prolog::sub_atom cl-prolog::abc ?before ?length 1 ?sub)
   :ordered (((?before . 0) (?length . 2) (?sub . cl-prolog::ab))
       ((?before . 1) (?length . 1) (?sub . cl-prolog::b))
       ((?before . 2) (?length . 0) (?sub . cl-prolog::||))))
  ((cl-prolog::sub_atom cl-prolog::abc ?before 3 0 ?sub)
   :ordered (((?before . 0) (?sub . cl-prolog::abc))))
  ((cl-prolog::sub_atom cl-prolog::abc 1 ?length ?after ?sub)
   :ordered (((?length . 0) (?after . 2) (?sub . cl-prolog::||))
       ((?length . 1) (?after . 1) (?sub . cl-prolog::b))
       ((?length . 2) (?after . 0) (?sub . cl-prolog::bc))))
  ((cl-prolog::sub_atom cl-prolog::abc 2 ?length 2 ?sub)
   :fails)
  ((cl-prolog::sub_atom cl-prolog::abc ?before 5 0 ?sub)
   :fails)
  ((cl-prolog::sub_atom cl-prolog::abc 3 3 ?after ?sub)
   :fails))

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
   :ordered (((?number . -0.125d0))))
  ((cl-prolog::atom_number cl-prolog::|42| 42) :succeeds)
  ((cl-prolog::atom_number cl-prolog::|42| 43) :fails)
  ((cl-prolog::atom_number cl-prolog::bad ?number) :fails))

(defun parse-number-codes (codes)
  (let ((solutions (query-prolog
                    (make-rulebase)
                    `(cl-prolog::number_codes ?number ,codes))))
    (cdr (assoc '?number (first solutions)))))

(defun number-codes-error-type (codes)
  (query-error-summary
   (make-rulebase) `(cl-prolog::number_codes ?number ,codes)))

(defun round-trip-number-codes (number)
  (let* ((encoded (query-prolog
                   (make-rulebase)
                   `(cl-prolog::number_codes ,number ?codes)))
         (codes (cdr (assoc '?codes (first encoded)))))
    (parse-number-codes codes)))

(defun number-output-error-type (number)
  (query-error-summary
   (make-rulebase) `(cl-prolog::number_codes ,number ?codes)))

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
  (:equal 'prolog-domain-error (number-codes-error-type '(49 50 120)))
  (:equal 'prolog-resource-error
          (number-codes-error-type
           (map 'list #'char-code
                (format nil "1e~A" (make-string 40 :initial-element #\9)))))
  (:equal 'prolog-resource-error
          (number-codes-error-type
           (map 'list #'char-code "1.0e4095"))))

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

(deftest atom-list-conversion-rejects-a-variable-tail ()
  (handler-case
      (progn
        (cl-prolog::%character-list-text
         (cons (cl-prolog::%text-atom "a") '?tail)
         nil (cl-prolog::%iso-atom "ATOM_CHARS"))
        (error "Expected a variable tail to be rejected"))
    (prolog-instantiation-error (condition)
      (declare (ignore condition))
      (is t "A variable list tail must raise an instantiation error"))))

(deftest atom-list-conversion-rejects-an-improper-tail ()
  (handler-case
      (progn
        (cl-prolog::%character-list-text
         (cons (cl-prolog::%text-atom "a") (cl-prolog::%text-atom "b"))
         nil (cl-prolog::%iso-atom "ATOM_CHARS"))
        (error "Expected an improper tail to be rejected"))
    (prolog-type-error (condition)
      (declare (ignore condition))
      (is t "A non-cons, non-nil tail must raise a type error"))))

(deftest atom-list-conversion-rejects-a-variable-element ()
  (handler-case
      (progn
        (cl-prolog::%character-list-text
         (list (cl-prolog::%text-atom "a") '?element)
         nil (cl-prolog::%iso-atom "ATOM_CHARS"))
        (error "Expected a variable element to be rejected"))
    (prolog-instantiation-error (condition)
      (declare (ignore condition))
      (is t "An unbound list element must raise an instantiation error"))))

(deftest resource-limit-check-rejects-values-past-the-configured-limit ()
  (handler-case
      (progn
        (cl-prolog::%check-resource-limit
         5 3 "TEST_RESOURCE" nil (cl-prolog::%iso-atom "TEST") "over limit")
        (error "Expected the over-limit value to be rejected"))
    (prolog-resource-error (condition)
      (declare (ignore condition))
      (is t "A value past the configured limit must raise a resource error"))))

(defun atom-builtin-error-summary (goal)
  (query-error-summary (make-rulebase) goal :with-data t))

(deftest-table atom-builtins-report-iso-errors ()
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (atom-builtin-error-summary '(cl-prolog::atom_length ?atom ?length)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "ATOM" 1))
          (atom-builtin-error-summary '(cl-prolog::atom_length 1 ?length)))
  (:equal '(prolog-domain-error ("DOMAIN_ERROR" "NOT_LESS_THAN_ZERO" -1))
          (atom-builtin-error-summary '(cl-prolog::atom_length atom -1)))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (atom-builtin-error-summary '(cl-prolog::atom_concat ?a right ?whole)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "ATOM" 3))
          (atom-builtin-error-summary '(cl-prolog::atom_concat 3 right whole)))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (atom-builtin-error-summary '(cl-prolog::sub_atom ?atom 0 1 ?after ?sub)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "INTEGER" 1.5))
          (atom-builtin-error-summary '(cl-prolog::sub_atom abc 1.5 1 ?after ?sub)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "ATOM" 42))
          (atom-builtin-error-summary '(cl-prolog::sub_atom abc 0 1 ?after 42)))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (atom-builtin-error-summary '(cl-prolog::atom_chars ?atom ?chars)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "CHARACTER" "AB"))
          (atom-builtin-error-summary '(cl-prolog::atom_chars ?atom (ab))))
  (:equal '(prolog-type-error ("TYPE_ERROR" "INTEGER" "ATOM"))
          (atom-builtin-error-summary '(cl-prolog::atom_codes ?atom (atom))))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (atom-builtin-error-summary '(cl-prolog::char_code ?character ?code)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "CHARACTER" "AB"))
          (atom-builtin-error-summary '(cl-prolog::char_code ab ?code)))
  (:equal '(prolog-domain-error ("DOMAIN_ERROR" "CHARACTER_CODE" -1))
          (atom-builtin-error-summary '(cl-prolog::char_code ?character -1)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "NUMBER" "ATOM"))
          (atom-builtin-error-summary '(cl-prolog::number_chars atom ?chars)))
  (:equal '(prolog-domain-error ("DOMAIN_ERROR" "NUMBER_TEXT" "bad"))
          (atom-builtin-error-summary '(cl-prolog::number_codes ?number (98 97 100))))
  (:equal '(prolog-instantiation-error "INSTANTIATION_ERROR")
          (atom-builtin-error-summary '(cl-prolog::atom_number ?atom ?number)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "ATOM" 1))
          (atom-builtin-error-summary '(cl-prolog::atom_number 1 ?number)))
  (:equal '(prolog-type-error ("TYPE_ERROR" "NUMBER" "ATOM"))
          (atom-builtin-error-summary '(cl-prolog::atom_number ?atom atom)))
  (:equal '(prolog-domain-error ("DOMAIN_ERROR" "PROLOG_NUMBER" 1/2))
          (atom-builtin-error-summary '(cl-prolog::atom_number ?atom 1/2))))
