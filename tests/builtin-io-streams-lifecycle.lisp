;;;; Stream lifecycle: open/close, stream_property, set_stream_position,
;;;; char/byte builtins over binary and text streams, current-input/
;;;; current-output selection, and rulebase-copy isolation.  Uses the
;;;; with-io-rulebase fixture defined in builtin-io-terms.lisp; term
;;;; reading/writing semantics live there, and malformed-input/error-
;;;; rejection coverage lives in builtin-io-open-errors.lisp.

(in-package #:cl-prolog.tests)

(deftest io-builtins-use-rulebase-standard-streams ()
  (with-io-rulebase (rulebase input output) "a"
    (assert-query rulebase (cl-prolog::current_input ?stream) :succeeds)
    (assert-query rulebase (cl-prolog::get_char ?character)
                  :ordered (((?character . cl-prolog::|a|))))
    (assert-query rulebase (cl-prolog::at_end_of_stream) :succeeds)
    (assert-query rulebase (cl-prolog::put_char z) :succeeds)
    (assert-query rulebase (cl-prolog::nl) :succeeds)
    (assert-query rulebase (cl-prolog::flush_output) :succeeds)
    (is-equal (format nil "Z~%") (get-output-stream-string output))))

(deftest io-builtins-support-explicit-streams ()
  (with-io-rulebase (rulebase input output) "x"
    (assert-query rulebase
                  (cl-prolog::get_char cl-prolog::user_input ?character)
                  :ordered (((?character . cl-prolog::|x|))))
    (assert-query rulebase
                  (cl-prolog::put_char cl-prolog::user_output q)
                  :succeeds)
    (assert-query rulebase
                  (cl-prolog::at_end_of_stream cl-prolog::user_input)
                  :succeeds)))

(deftest io-end-of-stream-state-progresses-from-at-to-past ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query
     rulebase
     (cl-prolog::stream_property
      cl-prolog::user_input (cl-prolog::end_of_stream ?state))
     :ordered (((?state . cl-prolog::at))))
    (assert-query rulebase (cl-prolog::get_char ?character)
                  :ordered (((?character . cl-prolog::end_of_file))))
    (assert-query
     rulebase
     (cl-prolog::stream_property
      cl-prolog::user_input (cl-prolog::end_of_stream ?state))
     :ordered (((?state . cl-prolog::past))))
    (assert-query rulebase (cl-prolog::get_char ?character)
                  :ordered (((?character . cl-prolog::end_of_file))))))

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

(deftest io-close-with-force-option-validates-and-succeeds ()
  (with-io-rulebase (rulebase input output) ""
    (let* ((context (cl-prolog::rulebase-io-context rulebase))
           (stream (make-string-output-stream)))
      (cl-prolog::%register-prolog-stream!
       context stream :write :alias 'cl-prolog::temporary)
      (assert-query rulebase
                    (cl-prolog::close cl-prolog::temporary
                                      ((cl-prolog::force cl-prolog::true)))
                    :succeeds)
      (is (not (open-stream-p stream))))))

(deftest io-close-with-force-option-requires-instantiation ()
  (with-io-rulebase (rulebase input output) ""
    (let* ((context (cl-prolog::rulebase-io-context rulebase))
           (stream (make-string-output-stream)))
      (cl-prolog::%register-prolog-stream!
       context stream :write :alias 'cl-prolog::temporary)
      (assert-query rulebase
                    (cl-prolog::close cl-prolog::temporary
                                      ((cl-prolog::force ?f)))
                    :signals))))

(deftest io-close-with-force-option-rejects-non-boolean-value ()
  (with-io-rulebase (rulebase input output) ""
    (let* ((context (cl-prolog::rulebase-io-context rulebase))
           (stream (make-string-output-stream)))
      (cl-prolog::%register-prolog-stream!
       context stream :write :alias 'cl-prolog::temporary)
      (assert-query rulebase
                    (cl-prolog::close cl-prolog::temporary
                                      ((cl-prolog::force cl-prolog::maybe)))
                    :signals))))

(deftest io-stream-property-supports-enumeration-and-partial-properties ()
  (with-io-rulebase (rulebase input output) "abc"
    (assert-query
     rulebase
     (cl-prolog::stream_property
      cl-prolog::user_input (cl-prolog::mode ?mode))
     :ordered (((?mode . cl-prolog.user-atoms::read))))
    (assert-query
     rulebase
     (cl-prolog::stream_property
      ?stream (cl-prolog::alias cl-prolog::user_output))
     :ordered (((?stream . cl-prolog::user_output))))
    (assert-query
     rulebase
     (cl-prolog::stream_property
      cl-prolog::user_input (cl-prolog.user-atoms::type ?type))
     :ordered (((?type . cl-prolog::text))))
    (assert-query
     rulebase
     (cl-prolog::stream_property
      cl-prolog::user_input (cl-prolog::reposition ?value))
     :ordered (((?value . cl-prolog:true))))))

(deftest io-set-stream-position-repositions-input-stream ()
  (with-io-rulebase (rulebase input output) "abc"
    (assert-query rulebase (cl-prolog::get_char ?first)
                  :ordered (((?first . cl-prolog::|a|))))
    (assert-query rulebase
                  (cl-prolog::set_stream_position cl-prolog::user_input 0)
                  :succeeds)
    (assert-query rulebase (cl-prolog::get_char ?again)
                  :ordered (((?again . cl-prolog::|a|))))))

(deftest io-set-stream-position-clears-past-end-state ()
  (with-io-rulebase (rulebase input output) "a"
    (assert-query rulebase (cl-prolog::get_char ?character)
                  :ordered (((?character . cl-prolog::|a|))))
    (assert-query rulebase (cl-prolog::get_char ?character)
                  :ordered (((?character . cl-prolog::end_of_file))))
    (assert-query
     rulebase
     (cl-prolog::stream_property
      cl-prolog::user_input (cl-prolog::end_of_stream cl-prolog::past))
     :succeeds)
    (assert-query rulebase
                  (cl-prolog::set_stream_position cl-prolog::user_input 0)
                  :succeeds)
    (assert-query rulebase (cl-prolog::get_char ?again)
                  :ordered (((?again . cl-prolog::|a|))))))

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
                 :ordered (((?value . cl-prolog::|a|))))
   (assert-query rulebase
                 (cl-prolog::get_char ?value)
                 :ordered (((?value . cl-prolog::|a|)))))
  ("explicit stream"
   (assert-query rulebase
                 (cl-prolog::peek_char cl-prolog::user_input ?value)
                 :ordered (((?value . cl-prolog::|a|))))
   (assert-query rulebase
                 (cl-prolog::get_char cl-prolog::user_input ?value)
                 :ordered (((?value . cl-prolog::|a|))))))

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
                    :ordered (((?byte . 65))))
      (assert-query rulebase
                    (cl-prolog::get_byte cl-prolog::binary_input ?byte)
                    :ordered (((?byte . 65))))
      (assert-query rulebase
                    (cl-prolog::get_byte cl-prolog::binary_input ?byte)
                    :ordered (((?byte . 255))))
      (assert-query rulebase
                    (cl-prolog::at_end_of_stream cl-prolog::binary_input)
                    :succeeds)
      (assert-query rulebase
                    (cl-prolog::get_byte cl-prolog::binary_input ?byte)
                    :ordered (((?byte . -1))))
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

(deftest io-open-accepts-three-and-four-argument-forms ()
  (uiop:with-temporary-file (:pathname path :type "txt")
    (with-open-file (stream path :direction :output :if-exists :supersede)
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

(deftest io-open-close-and-current-output-follow-stream-selection ()
  (uiop:with-temporary-file (:pathname path :type "txt")
    (with-io-rulebase (rulebase input output) ""
      (let ((query
              (%read-prolog-query
               rulebase
               "open('~A', write, _, [alias(sel)]), ~
                set_output(sel), put_char(q), close(sel), ~
                current_output(Output), Output == user_output."
               (namestring path))))
        (is (prolog-succeeds-p rulebase query))))))

(deftest io-open-close-and-current-input-follow-stream-selection ()
  (uiop:with-temporary-file (:pathname path :type "txt")
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (write-string "q" stream))
    (with-io-rulebase (rulebase input output) ""
      (let ((query
              (%read-prolog-query
               rulebase
               "open('~A', read, _, [alias(sel)]), ~
                set_input(sel), get_char(Char), close(sel), ~
                current_input(Input), Input == user_input, ~
                Char == 'q'."
               (namestring path))))
        (is (prolog-succeeds-p rulebase query))))))

(deftest io-open-supports-binary-streams-and-append-mode ()
  (uiop:with-temporary-file (:pathname path :type "txt")
    (with-io-rulebase (rulebase input output) ""
      (let ((query
              (%read-prolog-query
               rulebase
               "open('~A', write, S1, [type(binary)]), close(S1), ~
                open('~A', append, S2), close(S2)."
               (namestring path) (namestring path))))
        (is (prolog-succeeds-p rulebase query))))))

(deftest io-byte-builtins-current-stream-forms-work-and-validate-type ()
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
      (assert-query rulebase (cl-prolog::set_input cl-prolog::binary_input)
                    :succeeds)
      (assert-query rulebase (cl-prolog::get_byte ?byte) :ordered (((?byte . 65))))
      (assert-query rulebase (cl-prolog::peek_byte ?byte) :ordered (((?byte . 255))))
      (assert-query rulebase (cl-prolog::set_output cl-prolog::binary_output)
                    :succeeds)
      (assert-query rulebase (cl-prolog::put_byte cl-prolog::not_an_integer)
                    :signals))))

(deftest io-put-char-rejects-a-binary-output-stream ()
  (with-binary-stream-rulebase
      (rulebase context input-path output-path) #(65)
    (let ((output (open output-path :direction :output :if-exists :supersede
                        :element-type '(unsigned-byte 8))))
      (cl-prolog::%register-prolog-stream!
       context output :write :type :binary :alias 'cl-prolog::binary_output)
      (assert-query rulebase
                    (cl-prolog::put_char cl-prolog::binary_output z)
                    :signals))))

(deftest io-at-end-of-stream-fails-when-not-at-end ()
  (with-io-rulebase (rulebase input output) "ab"
    (assert-query rulebase (cl-prolog::at_end_of_stream) :fails)
    (assert-query rulebase
                  (cl-prolog::at_end_of_stream cl-prolog::user_input)
                  :fails)))

(deftest io-stream-property-rejects-malformed-property-shapes ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog::stream_property
                   cl-prolog::user_input cl-prolog::not_a_property_list)
                  :signals)
    (assert-query rulebase
                  (cl-prolog::stream_property
                   cl-prolog::user_input (cl-prolog::mode extra argument))
                  :signals)
    (assert-query rulebase
                  (cl-prolog::stream_property
                   cl-prolog::user_input (123 cl-prolog::value))
                  :signals)))

(deftest io-stream-property-enumeration-skips-closed-streams ()
  (with-io-rulebase (rulebase input output) ""
    (let ((context (cl-prolog::rulebase-io-context rulebase))
          (stream (make-string-output-stream)))
      (cl-prolog::%register-prolog-stream!
       context stream :write :alias 'cl-prolog::doomed_stream)
      ;; Closing the underlying stream out of band leaves a stale entry in the
      ;; table; enumeration must filter it while still serving live streams.
      (close stream)
      (assert-query rulebase
                    (cl-prolog::stream_property ?stream (cl-prolog::mode ?mode))
                    :succeeds)
      (assert-query rulebase
                    (cl-prolog::stream_property
                     ?stream (cl-prolog::alias cl-prolog::doomed_stream))
                    :fails))))
