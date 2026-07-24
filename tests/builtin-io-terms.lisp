;;;; Term I/O semantics: read_term/write_term/write_canonical/read/write/
;;;; writeq variants, char_conversion, and end-of-file reporting for term
;;;; reads.  Defines the shared with-io-rulebase fixture used across this
;;;; file and its siblings; stream lifecycle (open/close, stream_property,
;;;; set_stream_position, current-input/output selection) lives in
;;;; builtin-io-streams-lifecycle.lisp, and malformed-input/error-rejection
;;;; coverage lives in builtin-io-open-errors.lisp.

(in-package #:cl-prolog.tests)

(defmacro with-io-rulebase ((rulebase input output) input-text &body body)
  `(let* ((,input (make-string-input-stream ,input-text))
          (,output (make-string-output-stream))
          (context (cl-prolog::make-prolog-io-context
                    :input ,input :output ,output
                    :error-output (make-string-output-stream)))
          (,rulebase (make-rulebase :io-context context)))
     (unwind-protect
          (progn ,@body)
       (cl-prolog::%close-all-owned-prolog-streams! context))))

(deftest io-builtins-report-eof-and-reject-unsupported-writer-options ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase (cl-prolog::get_char ?character)
                  :ordered (((?character . cl-prolog::end_of_file))))
    (assert-query rulebase
                  (cl-prolog::write_term cl-prolog::user_output hello
                                         ((cl-prolog::quoted maybe)))
                  :signals)))

(deftest io-read-term-current-stream-reports-variables ()
  (with-io-rulebase (rulebase input output) "pair(X, X, Y)."
    (assert-query
     rulebase
     (cl-prolog::read_term
      ?term ((cl-prolog::variables ?variables)
             (cl-prolog::variable_names ?names)))
     :ordered (((?term . (cl-prolog::pair cl-prolog::?x cl-prolog::?x cl-prolog::?y))
          (?variables . (cl-prolog::?x cl-prolog::?y))
          (?names . ((cl-prolog::= cl-prolog::|X| cl-prolog::?x)
                     (cl-prolog::= cl-prolog::|Y| cl-prolog::?y))))))))
(deftest io-read-term-preserves-quoted-question-atoms ()
  (with-io-rulebase (rulebase input output) "'?x'."
    (with-single-query-solution
        (solution solutions rulebase
         (list 'cl-prolog::read_term '?term
               (list (list 'cl-prolog::variables '?variables))))
      (let ((term (logic-substitute '?term solution))
            (variables (logic-substitute '?variables solution)))
        (is (eq (find-package '#:cl-prolog.user-atoms)
                (symbol-package term)))
        (is (not (logic-var-p term)))
        (is (null variables))))))

(deftest io-read-term-reports-named-singletons-only ()
  (with-io-rulebase (rulebase input output) "tuple(X, Y, X, _, Z)."
    (with-single-query-solution
        (solution solutions rulebase
         (list 'cl-prolog::read_term '?term
               (list (list 'cl-prolog::singletons '?singletons))))
      (let ((term (logic-substitute '?term solution))
            (singletons (logic-substitute '?singletons solution)))
        (destructuring-bind (functor x y repeated-x anonymous z) term
          (is (eq 'cl-prolog::tuple functor))
          (is (eq x repeated-x))
          (is (logic-var-p anonymous))
          (is-equal
           (list (list 'cl-prolog::= 'cl-prolog::|Y| y)
                 (list 'cl-prolog::= 'cl-prolog::|Z| z))
           singletons))))))

(deftest io-read-term-validates-syntax-error-policy ()
  (with-io-rulebase (rulebase input output) "broken( ."
    (assert-query rulebase
                  (cl-prolog::read_term
                   ?term ((cl-prolog::syntax_errors cl-prolog::fail)))
                  :fails))
  (with-io-rulebase (rulebase input output) "ok."
    (assert-query rulebase
                  (cl-prolog::read_term
                   ?term ((cl-prolog::syntax_errors unsupported)))
                  :signals)))

(deftest io-read-term-rejects-unsupported-options ()
  (with-io-rulebase (rulebase input output) "ok."
    (assert-query rulebase
                  (cl-prolog::read_term
                   ?term ((cl-prolog::bogus value)))
                  :signals)))

(deftest io-read-term-rejects-a-non-list-options-argument ()
  (with-io-rulebase (rulebase input output) "ok."
    (assert-query rulebase
                  (cl-prolog::read_term ?term cl-prolog::not_a_list)
                  :signals)))

(deftest io-read-term-rejects-a-malformed-option-shape ()
  (with-io-rulebase (rulebase input output) "ok."
    (assert-query rulebase
                  (cl-prolog::read_term ?term (cl-prolog::malformed_shape))
                  :signals)))

(deftest io-read-term-syntax-errors-are-catchable-iso-errors ()
  (with-io-rulebase (rulebase input output) "broken( ."
    (is
     (prolog-succeeds-p
      rulebase
      (%read-prolog-query
       rulebase
       "catch(read_term(_, []), error(syntax_error(_), context(read_term, _)), true).")))))

(deftest io-write-term-current-stream-honors-ignore-ops ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog::write_term
                   (cl-prolog::+ 1 2)
                   ((cl-prolog::quoted cl-prolog::true)
                    (cl-prolog::ignore_ops cl-prolog::true)
                    (cl-prolog::numbervars cl-prolog::false)))
                  :succeeds)
    (is-equal "'+'(1,2)" (get-output-stream-string output))))

(deftest io-write-term-current-stream-honors-quoted ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog::write_term
                   cl-prolog::|Mary Jane|
                   ((cl-prolog::quoted cl-prolog::true)))
                  :succeeds)
    (assert-query rulebase
                  (cl-prolog::write_term
                   cl-prolog::|Mary Jane|
                   ((cl-prolog::quoted cl-prolog::false)))
                  :succeeds)
    (is-equal "'Mary Jane'Mary Jane" (get-output-stream-string output))))

(deftest io-write-term-explicit-stream-honors-numbervars ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog::write_term
                   cl-prolog::user_output
                   (cl-prolog::$var 25)
                   ((cl-prolog::numbervars cl-prolog::true)))
                  :succeeds)
    (assert-query rulebase
                  (cl-prolog::write_term
                   cl-prolog::user_output
                   (cl-prolog::$var 26)
                   ((cl-prolog::numbervars cl-prolog::true)))
                  :succeeds)
    (assert-query rulebase
                  (cl-prolog::write_term
                   cl-prolog::user_output
                   (cl-prolog::$var 0)
                   ((cl-prolog::numbervars cl-prolog::false)
                    (cl-prolog::quoted cl-prolog::true)))
                  :succeeds)
    (is-equal "ZA1'$VAR'(0)" (get-output-stream-string output))))

(deftest io-write-term-combines-ignore-ops-and-quoting ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog::write_term
                   (cl-prolog::+ (cl-prolog::$var 0) cl-prolog::|Mary Jane|)
                   ((cl-prolog::quoted cl-prolog::false)
                    (cl-prolog::ignore_ops cl-prolog::true)
                    (cl-prolog::numbervars cl-prolog::true)))
                  :succeeds)
    (is-equal "+(A,Mary Jane)" (get-output-stream-string output))))

(deftest io-write-term-rejects-invalid-boolean-options ()
  (dolist (option '(cl-prolog::quoted cl-prolog::ignore_ops
                    cl-prolog::numbervars))
    (with-io-rulebase (rulebase input output) ""
      (signals-error
       (query-prolog rulebase
                     `(cl-prolog::write_term hello ((,option maybe))))))))

(deftest-io-variants io-read-write-facades-share-term-semantics
    ((rulebase input output) "pair(X, X).")
  ("current stream"
   (assert-query rulebase
                 (cl-prolog::read ?term)
                 :ordered (((?term . (cl-prolog::pair
                                cl-prolog::?x cl-prolog::?x)))))
   (assert-query rulebase
                 (cl-prolog::write (cl-prolog::$var 0))
                 :succeeds)
   (assert-query rulebase
                 (cl-prolog::writeq cl-prolog::|Mary Jane|)
                 :succeeds)
   (is-equal "A'Mary Jane'" (get-output-stream-string output)))
  ("explicit stream"
   (assert-query rulebase
                 (cl-prolog::read cl-prolog::user_input ?term)
                 :ordered (((?term . (cl-prolog::pair
                                cl-prolog::?x cl-prolog::?x)))))
   (assert-query rulebase
                 (cl-prolog::write cl-prolog::user_output
                                   (cl-prolog::$var 0))
                 :succeeds)
   (assert-query rulebase
                 (cl-prolog::writeq cl-prolog::user_output
                                    cl-prolog::|Mary Jane|)
                 :succeeds)
   (is-equal "A'Mary Jane'" (get-output-stream-string output))))

(deftest io-char-conversion-applies-to-unquoted-read-text ()
  (with-io-rulebase (rulebase input output) "aaa. 'aaa'. aaa."
    (assert-query rulebase
                  (cl-prolog::char_conversion cl-prolog::|a| cl-prolog::|b|)
                  :succeeds)
    ;; The conversion table is inert until the flag is switched on.
    (assert-query rulebase (cl-prolog::read_term ?term ())
                  :ordered (((?term . cl-prolog::aaa))))
    (assert-query rulebase
                  (cl-prolog::set_prolog_flag cl-prolog::char_conversion on)
                  :succeeds)
    ;; Quoted atoms are exempt from conversion.
    (assert-query rulebase (cl-prolog::read_term ?term ())
                  :ordered (((?term . cl-prolog::|aaa|))))
    (assert-query rulebase (cl-prolog::read_term ?term ())
                  :ordered (((?term . cl-prolog::bbb))))))

(deftest io-write-canonical-round-trips-quoted-structure ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog::write_canonical
                   (cl-prolog::foo (cl-prolog::bar 1 2) cl-prolog::|a b|))
                  :succeeds)
    (let ((text (get-output-stream-string output)))
      (is (search "'a b'" text)
          "write_canonical must quote atoms that need quoting")
      (is-equal '(cl-prolog::foo (cl-prolog::bar 1 2) cl-prolog::|a b|)
                (read-prolog-term (concatenate 'string text " ."))))))

(deftest io-read-resource-errors-remain-catchable-for-all-syntax-policies ()
  (dolist
      (query-source
       (list
        "catch(read(_), error(resource_error(identifier_length), _), true)."
        "catch(read_term(_, []), error(resource_error(identifier_length), _), true)."
        "catch(read_term(_, [syntax_errors(fail)]), error(resource_error(identifier_length), _), true)."
        "catch(read_term(_, [syntax_errors(quiet)]), error(resource_error(identifier_length), _), true)."))
    (with-io-rulebase (rulebase input output) "toolong."
      (let ((query (%read-prolog-query rulebase query-source)))
        (let ((*max-prolog-identifier-length* 1))
          (is (prolog-succeeds-p rulebase query)))))))

(deftest io-dual-builtin-macro-rejects-malformed-clauses ()
  (signals-error
    (macroexpand-1
     '(cl-prolog::%define-io-dual-builtin
       (bogus_io_test_builtin () () "BOGUS")
       (rulebase environment depth emit)
       :wrong-key form
       :explicit form))))

(deftest io-read-term-and-peek-char-report-end-of-file ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase (cl-prolog::read_term ?term ())
                  :ordered (((?term . cl-prolog::end_of_file))))
    (assert-query rulebase (cl-prolog::peek_char ?character)
                  :ordered (((?character . cl-prolog::end_of_file))))))

(deftest io-explicit-stream-variants-cover-read-write-and-control-goals ()
  (with-io-rulebase (rulebase input output) ""
    (let ((context (cl-prolog::rulebase-io-context rulebase))
          (extra-input (make-string-input-stream "term_a."))
          (sink (make-string-output-stream)))
      (cl-prolog::%register-prolog-stream!
       context extra-input :read :alias 'cl-prolog::explicit_input)
      (cl-prolog::%register-prolog-stream!
       context sink :write :alias 'cl-prolog::explicit_output)
      (assert-query rulebase
                    (cl-prolog::read_term
                     cl-prolog::explicit_input ?term ())
                    :ordered (((?term . cl-prolog::term_a))))
      (assert-query rulebase
                    (cl-prolog::write_canonical
                     cl-prolog::explicit_output cl-prolog::term_a)
                    :succeeds)
      (assert-query rulebase (cl-prolog::nl cl-prolog::explicit_output)
                    :succeeds)
      (assert-query rulebase (cl-prolog::flush_output cl-prolog::explicit_output)
                    :succeeds))))
