(in-package #:cl-prolog)

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

(defun %io-write-byte-goal (entry byte environment emit operation)
  (%io-require-stream-type entry :binary environment operation)
  (write-byte (%io-byte byte environment operation)
              (prolog-stream-stream entry))
  (funcall emit environment))

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

(defun %io-byte-input-goal (entry byte environment emit operation peek)
  (%unify-emit byte (%io-read-byte entry environment operation :peek peek)
               environment emit))

(defun %io-byte (term environment operation)
  (let ((value (%io-resolve-term term environment operation)))
    (unless (integerp value)
      (%raise-type-error "INTEGER" value environment operation
                         "Byte must be an integer"))
    (unless (<= 0 value 255)
      (%raise-domain-error "BYTE" value environment operation
                           "Byte must be between 0 and 255"))
    value))

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
