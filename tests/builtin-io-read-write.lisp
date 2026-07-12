(in-package #:cl-prolog.tests)

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
