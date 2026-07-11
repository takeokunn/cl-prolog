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

(deftest io-character-lookahead-supports-current-and-explicit-streams ()
  (dolist (explicitp '(nil t))
    (with-io-rulebase (rulebase input output) "ab"
      (if explicitp
          (progn
            (assert-query rulebase
                          (cl-prolog::peek_char cl-prolog::user_input ?value)
                          => (((?value . cl-prolog::|a|))))
            (assert-query rulebase
                          (cl-prolog::get_char cl-prolog::user_input ?value)
                          => (((?value . cl-prolog::|a|)))))
          (progn
            (assert-query rulebase
                          (cl-prolog::peek_char ?value)
                          => (((?value . cl-prolog::|a|))))
            (assert-query rulebase
                          (cl-prolog::get_char ?value)
                          => (((?value . cl-prolog::|a|)))))))))

(deftest io-read-write-facades-share-term-semantics ()
  (dolist (explicitp '(nil t))
    (with-io-rulebase (rulebase input output) "pair(X, X)."
      (if explicitp
          (progn
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
                          :succeeds))
          (progn
            (assert-query rulebase
                          (cl-prolog::read ?term)
                          => (((?term . (cl-prolog::pair
                                         cl-prolog::?x cl-prolog::?x)))))
            (assert-query rulebase
                          (cl-prolog::write (cl-prolog::$var 0))
                          :succeeds)
            (assert-query rulebase
                          (cl-prolog::writeq cl-prolog::|Mary Jane|)
                          :succeeds)))
      (is-equal "A'Mary Jane'" (get-output-stream-string output)))))

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
