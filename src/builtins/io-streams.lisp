;;;; ISO stream read/write builtins.

(in-package #:cl-prolog)

(defun %io-read-term-values (entry operator-table mode environment operation)
  (let ((input (prolog-stream-stream entry)))
    (if (eq (peek-char t input nil :eof) :eof)
        (progn
          (setf (prolog-stream-end-of-stream entry) :past)
          (values (%iso-atom "end_of_file") '() '() '() t))
        (handler-case
            (multiple-value-bind (term variables names singletons)
                (%io-parse-term-with-variables input operator-table)
              (setf (prolog-stream-end-of-stream entry) :not)
              (values term variables names singletons t))
          (prolog-parser-resource-error (condition)
            (%raise-parser-resource-error condition environment operation))
          (prolog-parse-error (condition)
            (if (%io-option-name-p mode "error")
                (%raise-syntax-error condition environment operation)
                (values nil nil nil nil nil)))))))

(defun %io-unify-read-results (term value options variables names singletons
                               environment emit)
  (multiple-value-bind (term-environment term-ok)
      (unify term value environment)
    (when term-ok
      (multiple-value-bind (variables-environment variables-ok)
          (unify (%io-option "variables" options variables)
                 variables term-environment)
        (when variables-ok
          (multiple-value-bind (names-environment names-ok)
              (unify (%io-option "variable_names" options names)
                     names variables-environment)
            (when names-ok
              (multiple-value-bind (singletons-environment singletons-ok)
                  (unify (%io-option "singletons" options singletons)
                         singletons names-environment)
                (when singletons-ok (funcall emit singletons-environment))))))))))

(defun %io-read-term-goal (rulebase entry term options environment emit
                           &optional (operation (%io-operation "READ_TERM")))
  (let* ((parsed (%io-read-options options environment operation))
         (mode (%io-syntax-errors-mode parsed environment operation))
         (*active-char-conversions* (%rulebase-active-char-conversions rulebase)))
    (multiple-value-bind (value variables names singletons readablep)
        (%io-read-term-values entry (rulebase-operator-table rulebase) mode
                              environment operation)
      (when readablep
        (%io-unify-read-results term value parsed variables names singletons
                                environment emit)))))

(%define-io-dual-builtin (read_term (term options) (term options) "READ_TERM")
    (rulebase environment depth emit)
  :current
  (%io-read-term-goal
   rulebase (%io-current-input-entry rulebase)
   term options environment emit)
  :explicit
  (%io-read-term-goal
   rulebase (%io-stream-entry rulebase stream :input environment operation)
   term options environment emit))

(%define-io-dual-builtin (read (term) (term) "READ")
    (rulebase environment depth emit)
  :current
  (%io-read-term-goal
   rulebase (%io-current-input-entry rulebase)
   term '() environment emit operation)
  :explicit
  (%io-read-term-goal
   rulebase (%io-stream-entry rulebase stream :input environment operation)
   term '() environment emit operation))

(defun %io-write-term-goal (entry term options environment emit
                            &optional (operation (%io-operation "WRITE_TERM")))
  (let* (
         (parsed (%io-options options environment operation
                              '("quoted" "ignore_ops" "numbervars")))
         (ignore-ops (%io-boolean
                      (%io-option "ignore_ops" parsed (%iso-atom "false"))
                      environment operation))
         (quoted (%io-boolean (%io-option "quoted" parsed (%iso-atom "false"))
                              environment operation))
         (numbervars
           (%io-boolean (%io-option "numbervars" parsed (%iso-atom "false"))
                        environment operation)))
    (let ((value (logic-substitute term environment))
          (stream (prolog-stream-stream entry)))
      (%write-prolog-term-with-options value stream
                                       :quoted quoted
                                       :numbervars numbervars
                                       :ignore-ops ignore-ops))
    (funcall emit environment)))

(%define-io-dual-builtin (write_term (term options) (term options) "WRITE_TERM")
    (rulebase environment depth emit)
  :current
  (%io-write-term-goal
   (%io-current-output-entry rulebase)
   term options environment emit)
  :explicit
  (%io-write-term-goal
   (%io-stream-entry rulebase stream :output environment operation)
   term options environment emit))

(defun %io-write-facade-goal (entry term quoted environment emit operation)
  (%io-write-term-goal
   entry term
   (list (list (%iso-atom "quoted") (%iso-atom (if quoted "true" "false")))
         (list (%iso-atom "numbervars") (%iso-atom "true")))
   environment emit operation))

(%define-io-dual-builtin (write (term) (term) "WRITE")
    (rulebase environment depth emit)
  :current
  (%io-write-facade-goal
   (%io-current-output-entry rulebase)
   term nil environment emit operation)
  :explicit
  (%io-write-facade-goal
   (%io-stream-entry rulebase stream :output environment operation)
   term nil environment emit operation))

(%define-io-dual-builtin (writeq (term) (term) "WRITEQ")
    (rulebase environment depth emit)
  :current
  (%io-write-facade-goal
   (%io-current-output-entry rulebase)
   term t environment emit operation)
  :explicit
  (%io-write-facade-goal
   (%io-stream-entry rulebase stream :output environment operation)
   term t environment emit operation))

(defun %io-write-canonical-goal (entry term environment emit operation)
  ;; ISO write_canonical/1,2: quoted, operator-free, no numbervars, so the
  ;; output reads back as the same term under any operator table.
  (%io-write-term-goal
   entry term
   (list (list (%iso-atom "quoted") (%iso-atom "true"))
         (list (%iso-atom "ignore_ops") (%iso-atom "true")))
   environment emit operation))

(%define-io-dual-builtin (write_canonical (term) (term) "WRITE_CANONICAL")
    (rulebase environment depth emit)
  :current
  (%io-write-canonical-goal
   (%io-current-output-entry rulebase)
   term environment emit operation)
  :explicit
  (%io-write-canonical-goal
   (%io-stream-entry rulebase stream :output environment operation)
   term environment emit operation))

(defun %io-newline (rulebase entry)
  (terpri (prolog-stream-stream
           (or entry (prolog-io-context-current-output (%io-context rulebase))))))

(defun %io-newline-goal (rulebase stream environment emit)
  (%io-newline rulebase stream)
  (funcall emit environment))

(%define-io-dual-builtin (nl () () "NL")
    (rulebase environment depth emit)
  :current
  (%io-newline-goal rulebase nil environment emit)
  :explicit
  (%io-newline-goal
   rulebase
   (%io-stream-entry rulebase stream :output environment operation)
   environment emit))

(%define-io-dual-builtin (flush_output () () "FLUSH_OUTPUT")
    (rulebase environment depth emit)
  :current
  (%io-flush-goal rulebase nil environment emit)
  :explicit
  (%io-flush-goal
   rulebase
   (%io-stream-entry rulebase stream :output environment operation)
   environment emit))

(defun %io-flush (rulebase entry)
  (finish-output
   (prolog-stream-stream
    (or entry (prolog-io-context-current-output (%io-context rulebase))))))

(defun %io-flush-goal (rulebase stream environment emit)
  (%io-flush rulebase stream)
  (funcall emit environment))

(defun %io-require-stream-type (entry type environment operation)
  (unless (eq (prolog-stream-type entry) type)
    (%raise-permission-error
     (if (eq (prolog-stream-direction entry) :input) "INPUT" "OUTPUT")
     (if (eq type :text) "TEXT_STREAM" "BINARY_STREAM")
     (%io-public-designator entry) environment operation
     (if (eq type :text)
         "Character operation requires a text stream"
         "Byte operation requires a binary stream")))
  entry)

(defun %io-character-input (entry environment operation &key peek)
  (%io-require-stream-type entry :text environment operation)
  (let ((character (if peek
                       (peek-char nil (prolog-stream-stream entry) nil nil)
                       (read-char (prolog-stream-stream entry) nil nil))))
    (setf (prolog-stream-end-of-stream entry)
          (cond (character :not)
                (peek :at)
                (t :past)))
    (if character (%io-character-atom character) (%iso-atom "end_of_file"))))

(%define-io-dual-builtin (get_char (character) (character) "GET_CHAR")
    (rulebase environment depth emit)
  :current
  (%unify-emit character
               (%io-character-input
                (%io-current-input-entry rulebase)
                environment operation)
               environment emit)
  :explicit
  (%unify-emit character
               (%io-character-input
                (%io-stream-entry rulebase stream :input environment operation)
                environment operation)
               environment emit))

(%define-io-dual-builtin (peek_char (character) (character) "PEEK_CHAR")
    (rulebase environment depth emit)
  :current
  (%unify-emit
   character
   (%io-character-input
    (%io-current-input-entry rulebase)
    environment operation :peek t)
   environment emit)
  :explicit
  (%unify-emit
   character
   (%io-character-input
    (%io-stream-entry rulebase stream :input environment operation)
    environment operation :peek t)
   environment emit))

(defun %io-write-character (entry term environment operation)
  (%io-require-stream-type entry :text environment operation)
  (write-char (%io-character term environment operation)
              (prolog-stream-stream entry)))

(defun %io-read-byte (entry environment operation &key peek)
  (%io-require-stream-type entry :binary environment operation)
  (let* ((stream (prolog-stream-stream entry))
         (position (and peek (ignore-errors (file-position stream)))))
    (when (and peek (not (integerp position)))
      (%raise-permission-error
       "INPUT" "BINARY_STREAM" (%io-public-designator entry)
       environment operation "Binary stream does not support peeking"))
    (let ((byte (read-byte stream nil nil)))
      (when peek
        (unless (ignore-errors (file-position stream position))
          (%raise-permission-error
           "INPUT" "BINARY_STREAM" (%io-public-designator entry)
           environment operation "Binary stream position could not be restored")))
      (setf (prolog-stream-end-of-stream entry)
            (cond (byte :not)
                  (peek :at)
                  (t :past)))
      (or byte -1))))

(defun %io-byte (term environment operation)
  (let ((value (%io-resolve-term term environment operation)))
    (unless (integerp value)
      (%raise-type-error "INTEGER" value environment operation
                         "Byte must be an integer"))
    (unless (<= 0 value 255)
      (%raise-domain-error "BYTE" value environment operation
                           "Byte must be between 0 and 255"))
    value))

(defun %io-byte-input-goal (entry byte environment emit operation peek)
  (%unify-emit byte (%io-read-byte entry environment operation :peek peek)
               environment emit))

(%define-io-dual-builtin (put_char (character) (character) "PUT_CHAR")
    (rulebase environment depth emit)
  :current
  (progn
    (%io-write-character
     (%io-current-output-entry rulebase)
     character environment operation)
    (funcall emit environment))
  :explicit
  (progn
    (%io-write-character
     (%io-stream-entry rulebase stream :output environment operation)
     character environment operation)
    (funcall emit environment)))

(%define-io-dual-builtin (get_byte (byte) (byte) "GET_BYTE")
    (rulebase environment depth emit)
  :current
  (%io-byte-input-goal
   (%io-current-input-entry rulebase)
   byte environment emit operation nil)
  :explicit
  (%io-byte-input-goal
   (%io-stream-entry rulebase stream :input environment operation)
   byte environment emit operation nil))

(%define-io-dual-builtin (peek_byte (byte) (byte) "PEEK_BYTE")
    (rulebase environment depth emit)
  :current
  (%io-byte-input-goal
   (%io-current-input-entry rulebase)
   byte environment emit operation t)
  :explicit
  (%io-byte-input-goal
   (%io-stream-entry rulebase stream :input environment operation)
   byte environment emit operation t))

(defun %io-write-byte-goal (entry byte environment emit operation)
  (%io-require-stream-type entry :binary environment operation)
  (write-byte (%io-byte byte environment operation)
              (prolog-stream-stream entry))
  (funcall emit environment))
(%define-io-dual-builtin (put_byte (byte) (byte) "PUT_BYTE")
    (rulebase environment depth emit)
  :current
  (%io-write-byte-goal
   (%io-current-output-entry rulebase)
   byte environment emit operation)
  :explicit
  (%io-write-byte-goal
   (%io-stream-entry rulebase stream :output environment operation)
   byte environment emit operation))

(defun %io-at-end-p (entry environment operation)
  (if (eq (prolog-stream-end-of-stream entry) :past)
      t
      (let ((at-end-p
              (ecase (prolog-stream-type entry)
                (:text
                 (eq (peek-char nil (prolog-stream-stream entry) nil :eof) :eof))
                (:binary
                 (= -1 (%io-read-byte entry environment operation :peek t))))))
        (setf (prolog-stream-end-of-stream entry)
              (if at-end-p :at :not))
        at-end-p)))

(%define-io-dual-builtin (at_end_of_stream () () "AT_END_OF_STREAM")
    (rulebase environment depth emit)
  :current
  (when (%io-at-end-p
         (%io-current-input-entry rulebase)
         environment operation)
    (funcall emit environment))
  :explicit
  (when (%io-at-end-p
         (%io-stream-entry rulebase stream :input environment operation)
         environment operation)
    (funcall emit environment)))
