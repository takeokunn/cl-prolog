;;;; Query-level contract for ISO-style stream builtins.

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

(deftest io-builtins-use-rulebase-standard-streams ()
  (with-io-rulebase (rulebase input output) "a"
    (assert-query rulebase (cl-prolog::current_input ?stream) :succeeds)
    (assert-query rulebase (cl-prolog::get_char ?character)
                  => (((?character . cl-prolog::|a|))))
    (assert-query rulebase (cl-prolog::at_end_of_stream) :succeeds)
    (assert-query rulebase (cl-prolog::put_char z) :succeeds)
    (assert-query rulebase (cl-prolog::nl) :succeeds)
    (assert-query rulebase (cl-prolog::flush_output) :succeeds)
    (is-equal (format nil "Z~%") (get-output-stream-string output))))

(deftest io-builtins-support-explicit-streams ()
  (with-io-rulebase (rulebase input output) "x"
    (assert-query rulebase
                  (cl-prolog::get_char cl-prolog::user_input ?character)
                  => (((?character . cl-prolog::|x|))))
    (assert-query rulebase
                  (cl-prolog::put_char cl-prolog::user_output q)
                  :succeeds)
    (assert-query rulebase
                  (cl-prolog::at_end_of_stream cl-prolog::user_input)
                  :succeeds)))

(deftest io-builtins-report-eof-and-reject-unsupported-writer-options ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase (cl-prolog::get_char ?character)
                  => (((?character . cl-prolog::end_of_file))))
    (assert-query rulebase
                  (cl-prolog::write_term cl-prolog::user_output hello
                                         ((cl-prolog::quoted maybe)))
                  :signals)))

(deftest io-end-of-stream-state-progresses-from-at-to-past ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query
     rulebase
     (cl-prolog::stream_property
      cl-prolog::user_input (cl-prolog::end_of_stream ?state))
     => (((?state . cl-prolog::at))))
    (assert-query rulebase (cl-prolog::get_char ?character)
                  => (((?character . cl-prolog::end_of_file))))
    (assert-query
     rulebase
     (cl-prolog::stream_property
      cl-prolog::user_input (cl-prolog::end_of_stream ?state))
     => (((?state . cl-prolog::past))))
    (assert-query rulebase (cl-prolog::get_char ?character)
                  => (((?character . cl-prolog::end_of_file))))))

(deftest io-read-term-current-stream-reports-variables ()
  (with-io-rulebase (rulebase input output) "pair(X, X, Y)."
    (assert-query
     rulebase
     (cl-prolog::read_term
      ?term ((cl-prolog::variables ?variables)
             (cl-prolog::variable_names ?names)))
     => (((?term . (cl-prolog::pair cl-prolog::?x cl-prolog::?x cl-prolog::?y))
          (?variables . (cl-prolog::?x cl-prolog::?y))
          (?names . ((cl-prolog::= cl-prolog::|X| cl-prolog::?x)
                     (cl-prolog::= cl-prolog::|Y| cl-prolog::?y))))))))

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

(deftest io-read-term-syntax-errors-are-catchable-iso-errors ()
  (with-io-rulebase (rulebase input output) "broken( ."
    (is
     (prolog-succeeds-p
      rulebase
      (read-prolog-term
       "catch(read_term(_, []), error(syntax_error(_), context(read_term, _)), true)."
       (cl-prolog::rulebase-operator-table rulebase))))))

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

(deftest rulebase-copies-isolate-io-context ()
  (with-io-rulebase (rulebase input output) ""
    (let ((copy (cl-prolog::%copy-rulebase rulebase)))
      (is (not (eq (cl-prolog::rulebase-io-context rulebase)
                   (cl-prolog::rulebase-io-context copy)))))))

(deftest io-close-one-argument-closes-and-forgets-owned-stream ()
  (with-io-rulebase (rulebase input output) ""
    (let* ((context (cl-prolog::rulebase-io-context rulebase))
           (stream (make-string-output-stream)))
      (cl-prolog::%register-prolog-stream!
       context stream :write :alias 'cl-prolog::temporary)
      (assert-query rulebase (cl-prolog::close cl-prolog::temporary) :succeeds)
      (is (not (open-stream-p stream)))
      (assert-query rulebase
                    (cl-prolog::stream_property cl-prolog::temporary ?property)
                    :signals))))

(deftest io-stream-property-supports-enumeration-and-partial-properties ()
  (with-io-rulebase (rulebase input output) "abc"
    (assert-query
     rulebase
     (cl-prolog::stream_property
      cl-prolog::user_input (cl-prolog::mode ?mode))
     => (((?mode . cl-prolog.user-atoms::read))))
    (assert-query
     rulebase
     (cl-prolog::stream_property
      ?stream (cl-prolog::alias cl-prolog::user_output))
     => (((?stream . cl-prolog::user_output))))
    (assert-query
     rulebase
     (cl-prolog::stream_property
      cl-prolog::user_input (cl-prolog.user-atoms::type ?type))
     => (((?type . cl-prolog::text))))
    (assert-query
     rulebase
     (cl-prolog::stream_property
      cl-prolog::user_input (cl-prolog::reposition ?value))
     => (((?value . cl-prolog:true))))))

(deftest io-set-stream-position-repositions-input-stream ()
  (with-io-rulebase (rulebase input output) "abc"
    (assert-query rulebase (cl-prolog::get_char ?first)
                  => (((?first . cl-prolog::|a|))))
    (assert-query rulebase
                  (cl-prolog::set_stream_position cl-prolog::user_input 0)
                  :succeeds)
    (assert-query rulebase (cl-prolog::get_char ?again)
                  => (((?again . cl-prolog::|a|))))))

(deftest io-set-stream-position-clears-past-end-state ()
  (with-io-rulebase (rulebase input output) "a"
    (assert-query rulebase (cl-prolog::get_char ?character)
                  => (((?character . cl-prolog::|a|))))
    (assert-query rulebase (cl-prolog::get_char ?character)
                  => (((?character . cl-prolog::end_of_file))))
    (assert-query
     rulebase
     (cl-prolog::stream_property
      cl-prolog::user_input (cl-prolog::end_of_stream cl-prolog::past))
     :succeeds)
    (assert-query rulebase
                  (cl-prolog::set_stream_position cl-prolog::user_input 0)
                  :succeeds)
    (assert-query rulebase (cl-prolog::get_char ?again)
                  => (((?again . cl-prolog::|a|))))))

(deftest io-stream-position-validates-property-and-position ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog::stream_property
                   cl-prolog::user_input (cl-prolog::unknown value))
                  :signals)
    (assert-query rulebase
                  (cl-prolog::set_stream_position cl-prolog::user_input atom)
                  :signals)
    (assert-query rulebase
                  (cl-prolog::set_stream_position cl-prolog::user_input -1)
                  :signals)))

(deftest-io-variants io-character-lookahead-supports-current-and-explicit-streams
    ((rulebase input output) "ab")
  ("current stream"
   (assert-query rulebase
                 (cl-prolog::peek_char ?value)
                 => (((?value . cl-prolog::|a|))))
   (assert-query rulebase
                 (cl-prolog::get_char ?value)
                 => (((?value . cl-prolog::|a|)))))
  ("explicit stream"
   (assert-query rulebase
                 (cl-prolog::peek_char cl-prolog::user_input ?value)
                 => (((?value . cl-prolog::|a|))))
   (assert-query rulebase
                 (cl-prolog::get_char cl-prolog::user_input ?value)
                 => (((?value . cl-prolog::|a|))))))

(deftest-io-variants io-read-write-facades-share-term-semantics
    ((rulebase input output) "pair(X, X).")
  ("current stream"
   (assert-query rulebase
                 (cl-prolog::read ?term)
                 => (((?term . (cl-prolog::pair
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
                 => (((?term . (cl-prolog::pair
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

(defmacro with-binary-stream-rulebase
    ((rulebase context input-path output-path) input-bytes &body body)
  `(let* ((,input-path
            (merge-pathnames
             (format nil "cl-prolog-input-~36R.bin" (random (expt 36 8)))
             #p"/tmp/"))
          (,output-path
            (merge-pathnames
             (format nil "cl-prolog-output-~36R.bin" (random (expt 36 8)))
             #p"/tmp/")))
     (unwind-protect
          (progn
            (with-open-file (stream ,input-path :direction :output
                                    :if-exists :supersede
                                    :element-type '(unsigned-byte 8))
              (write-sequence ,input-bytes stream))
            (let* ((,context (cl-prolog::make-prolog-io-context
                              :input (make-string-input-stream "")
                              :output (make-string-output-stream)
                              :error-output (make-string-output-stream)))
                   (,rulebase (make-rulebase :io-context ,context)))
              (unwind-protect
                   (progn ,@body)
                (cl-prolog::%close-all-owned-prolog-streams! ,context))))
       (ignore-errors (delete-file ,input-path))
       (ignore-errors (delete-file ,output-path)))))

(deftest io-byte-builtins-support-binary-streams-and-eof ()
  (with-binary-stream-rulebase
      (rulebase context input-path output-path) #(65 255)
    (let ((input (open input-path :direction :input
                       :element-type '(unsigned-byte 8)))
          (output (open output-path :direction :output :if-exists :supersede
                        :element-type '(unsigned-byte 8))))
      (cl-prolog::%register-prolog-stream!
       context input :read :type :binary :alias 'cl-prolog::binary_input)
      (cl-prolog::%register-prolog-stream!
       context output :write :type :binary :alias 'cl-prolog::binary_output)
      (assert-query rulebase
                    (cl-prolog::peek_byte cl-prolog::binary_input ?byte)
                    => (((?byte . 65))))
      (assert-query rulebase
                    (cl-prolog::get_byte cl-prolog::binary_input ?byte)
                    => (((?byte . 65))))
      (assert-query rulebase
                    (cl-prolog::get_byte cl-prolog::binary_input ?byte)
                    => (((?byte . 255))))
      (assert-query rulebase
                    (cl-prolog::at_end_of_stream cl-prolog::binary_input)
                    :succeeds)
      (assert-query rulebase
                    (cl-prolog::get_byte cl-prolog::binary_input ?byte)
                    => (((?byte . -1))))
      (assert-query rulebase
                    (cl-prolog::put_byte cl-prolog::binary_output 128)
                    :succeeds)
      (assert-query rulebase
                    (cl-prolog::put_byte cl-prolog::binary_output 256)
                    :signals)
      (cl-prolog::%close-all-owned-prolog-streams! context)
      (with-open-file (stream output-path :direction :input
                             :element-type '(unsigned-byte 8))
        (is-equal '(128) (loop for byte = (read-byte stream nil nil)
                               while byte collect byte))))))

(deftest io-character-and-byte-builtins-reject-wrong-stream-types ()
  (with-binary-stream-rulebase
      (rulebase context input-path output-path) #(65)
    (let ((input (open input-path :direction :input
                       :element-type '(unsigned-byte 8))))
      (cl-prolog::%register-prolog-stream!
       context input :read :type :binary :alias 'cl-prolog::binary_input)
      (assert-query rulebase
                    (cl-prolog::get_char cl-prolog::binary_input ?character)
                    :signals)
      (assert-query rulebase
                    (cl-prolog::get_byte cl-prolog::user_input ?byte)
                    :signals))))

(deftest io-char-conversion-applies-to-unquoted-read-text ()
  (with-io-rulebase (rulebase input output) "aaa. 'aaa'. aaa."
    (assert-query rulebase
                  (cl-prolog::char_conversion cl-prolog::|a| cl-prolog::|b|)
                  :succeeds)
    ;; The conversion table is inert until the flag is switched on.
    (assert-query rulebase (cl-prolog::read_term ?term ())
                  => (((?term . cl-prolog::aaa))))
    (assert-query rulebase
                  (cl-prolog::set_prolog_flag cl-prolog::char_conversion on)
                  :succeeds)
    ;; Quoted atoms are exempt from conversion.
    (assert-query rulebase (cl-prolog::read_term ?term ())
                  => (((?term . cl-prolog::|aaa|))))
    (assert-query rulebase (cl-prolog::read_term ?term ())
                  => (((?term . cl-prolog::bbb))))))

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

(deftest io-open-accepts-three-and-four-argument-forms ()
  (let ((path (merge-pathnames (format nil "cl-prolog-open3-~D.txt"
                                       (sb-unix:unix-getpid))
                               #p"/tmp/")))
    (unwind-protect
         (progn
           (with-open-file (stream path :direction :output
                                   :if-exists :supersede)
             (write-string "hello." stream))
           (with-io-rulebase (rulebase input output) ""
             (let ((source (cl-prolog::%prolog-atom-symbol (namestring path)
                                                           :preserve-case t)))
               (dolist (query (list (list 'cl-prolog::open source
                                          'cl-prolog.user-atoms::read '?stream)
                                    (list 'cl-prolog::open source
                                          'cl-prolog.user-atoms::read '?stream '())))
                 (with-single-query-solution (solution solutions rulebase query)
                   (is (not (null (solution-binding '?stream solution)))
                       "open must bind a stream")
                   (assert-query rulebase (cl-prolog::stream_property
                                           ?stream (cl-prolog::mode ?mode))
                                 :succeeds)))))))
      (ignore-errors (delete-file path))))
