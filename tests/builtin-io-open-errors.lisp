;;;; Malformed-input and error-rejection coverage: open rejecting non-atom
;;;; sources/aliases, missing files/parent directories, unsupported type
;;;; options, rollback on duplicate alias, boolean-option/non-symbol/non-
;;;; boolean rejections, stream-designator-does-not-unify, and unseekable-
;;;; stream reposition reporting.  Uses the with-io-rulebase fixture defined
;;;; in builtin-io-terms.lisp; stream lifecycle lives in
;;;; builtin-io-streams-lifecycle.lisp.

(in-package #:cl-prolog.tests)

(deftest io-boolean-option-rejects-an-unbound-value ()
  (handler-case
      (progn
        (cl-prolog::%io-boolean '?value nil (cl-prolog::%iso-atom "TEST"))
        (error "Expected an unbound boolean option to be rejected"))
    (prolog-instantiation-error (condition)
      (declare (ignore condition))
      (is t "An unbound boolean option must raise an instantiation error"))))

(deftest io-read-term-rejects-an-unbound-syntax-errors-value ()
  (with-io-rulebase (rulebase input output) "ok."
    (assert-query rulebase
                  (cl-prolog::read_term ?term ((cl-prolog::syntax_errors ?policy)))
                  :signals)))

(deftest io-open-rejects-a-non-atom-source ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog::open 123 cl-prolog.user-atoms::read ?stream)
                  :signals)))

(deftest io-open-reports-existence-error-for-a-missing-unquoted-source ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog::open
                   cl-prolog::nonexistent_io_open_test_source_xyz
                   cl-prolog.user-atoms::read ?stream)
                  :signals)))

(deftest io-open-rejects-an-unsupported-type-option ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog::open
                   cl-prolog::whatever cl-prolog.user-atoms::read ?stream
                   ((cl-prolog::type cl-prolog::bogus)))
                  :signals)))

(deftest io-open-rejects-a-non-atom-alias ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog::open
                   cl-prolog::whatever cl-prolog.user-atoms::read ?stream
                   ((cl-prolog::alias 123)))
                  :signals)))

(deftest io-open-reports-existence-error-for-a-missing-parent-directory ()
  (with-io-rulebase (rulebase input output) ""
    (let ((query
            (%read-prolog-query
             rulebase
             "open('/tmp/definitely_nonexistent_dir_xyz_123/file.pl', write, _).")))
      (signals-error (prolog-succeeds-p rulebase query)))))

(deftest io-open-rolls-back-a-newly-opened-stream-on-duplicate-alias ()
  (uiop:with-temporary-file (:pathname path-a :type "txt")
    (uiop:with-temporary-file (:pathname path-b :type "txt")
      (with-io-rulebase (rulebase input output) ""
        (let ((query
                (%read-prolog-query
                 rulebase
                 "open('~A', write, S1, [alias(dup_alias_test)]), ~
                  open('~A', write, S2, [alias(dup_alias_test)])."
                 (namestring path-a) (namestring path-b))))
          (signals-error (prolog-succeeds-p rulebase query)))))))

(deftest io-open-fails-when-the-stream-designator-does-not-unify ()
  (uiop:with-temporary-file (:pathname path :type "txt")
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (write-string "q" stream))
    (with-io-rulebase (rulebase input output) ""
      (let ((query
              (%read-prolog-query rulebase "open('~A', read, wrong_designator)."
                                   (namestring path))))
        (is (not (prolog-succeeds-p rulebase query)))))))

(deftest io-put-char-rejects-a-multi-character-atom ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase (cl-prolog::put_char cl-prolog::ab) :signals)))

(defclass unseekable-binary-input-stream (sb-gray:fundamental-binary-input-stream)
  ((bytes :initarg :bytes :accessor unseekable-binary-input-stream-bytes))
  (:documentation "A binary stream with no FILE-POSITION support, for exercising
peek_byte/1's fallback when a stream cannot save and restore its position."))

(defmethod sb-gray:stream-read-byte ((stream unseekable-binary-input-stream))
  (if (unseekable-binary-input-stream-bytes stream)
      (pop (unseekable-binary-input-stream-bytes stream))
      :eof))

(deftest io-peek-byte-rejects-a-stream-without-file-position-support ()
  (with-io-rulebase (rulebase input output) ""
    (let ((context (cl-prolog::rulebase-io-context rulebase))
          (stream (make-instance 'unseekable-binary-input-stream
                                 :bytes (list 65))))
      (cl-prolog::%register-prolog-stream!
       context stream :read :type :binary :alias 'cl-prolog::unseekable_input)
      (assert-query rulebase
                    (cl-prolog::peek_byte cl-prolog::unseekable_input ?byte)
                    :signals))))

(deftest io-set-stream-position-rejects-a-stream-without-file-position-support ()
  (with-io-rulebase (rulebase input output) ""
    (let ((context (cl-prolog::rulebase-io-context rulebase))
          (stream (make-instance 'unseekable-binary-input-stream
                                 :bytes (list 65))))
      (cl-prolog::%register-prolog-stream!
       context stream :read :type :binary :alias 'cl-prolog::unseekable_input2)
      (assert-query rulebase
                    (cl-prolog::set_stream_position
                     cl-prolog::unseekable_input2 0)
                    :signals))))

(deftest io-boolean-option-rejects-a-non-symbol-value ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog::write_term hello ((cl-prolog::quoted 5)))
                  :signals)))

(deftest io-read-term-rejects-a-non-symbol-syntax-errors-value ()
  (with-io-rulebase (rulebase input output) "ok."
    (assert-query rulebase
                  (cl-prolog::read_term ?term ((cl-prolog::syntax_errors 123)))
                  :signals)))

(deftest io-put-char-rejects-a-non-symbol-character ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase (cl-prolog::put_char 123) :signals)))

(deftest io-open-rejects-a-non-symbol-type-option ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog::open
                   cl-prolog::whatever cl-prolog.user-atoms::read ?stream
                   ((cl-prolog::type 123)))
                  :signals)))

(defclass unseekable-character-input-stream
    (sb-gray:fundamental-character-input-stream)
  ((characters :initarg :characters
               :accessor unseekable-character-input-stream-characters))
  (:documentation "A character stream with no FILE-POSITION support, for
exercising stream_property/2 on a stream that cannot report a position."))

(defmethod sb-gray:stream-read-char ((stream unseekable-character-input-stream))
  (if (unseekable-character-input-stream-characters stream)
      (pop (unseekable-character-input-stream-characters stream))
      :eof))

(defmethod sb-gray:stream-peek-char ((stream unseekable-character-input-stream))
  (if (unseekable-character-input-stream-characters stream)
      (car (unseekable-character-input-stream-characters stream))
      :eof))

(deftest io-stream-property-reports-false-reposition-for-unseekable-stream ()
  (with-io-rulebase (rulebase input output) ""
    (let ((context (cl-prolog::rulebase-io-context rulebase))
          (stream (make-instance 'unseekable-character-input-stream
                                 :characters (list #\a))))
      (cl-prolog::%register-prolog-stream!
       context stream :read :type :text :alias 'cl-prolog::unseekable_prop)
      (assert-query rulebase
                    (cl-prolog::stream_property
                     cl-prolog::unseekable_prop (cl-prolog::reposition ?value))
                    :ordered (((?value . cl-prolog::false))))
      (assert-query rulebase
                    (cl-prolog::stream_property
                     cl-prolog::unseekable_prop
                     (cl-prolog.user-atoms::position ?p))
                    :fails))))
