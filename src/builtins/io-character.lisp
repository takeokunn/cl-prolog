(in-package #:cl-prolog)

(defun %io-write-character (entry term environment operation)
  (%io-require-stream-type entry :text environment operation)
  (write-char (%io-character term environment operation)
              (prolog-stream-stream entry)))

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

(defun %io-flush-goal (rulebase stream environment emit)
  (%io-flush rulebase stream)
  (funcall emit environment))

(defun %io-flush (rulebase entry)
  (finish-output
   (prolog-stream-stream
    (or entry (prolog-io-context-current-output (%io-context rulebase))))))

(%define-io-dual-builtin (flush_output () () "FLUSH_OUTPUT")
    (rulebase environment depth emit)
  :current
  (%io-flush-goal rulebase nil environment emit)
  :explicit
  (%io-flush-goal
   rulebase
   (%io-stream-entry rulebase stream :output environment operation)
   environment emit))

(%define-io-dual-builtin (nl () () "NL")
    (rulebase environment depth emit)
  :current
  (%io-newline-goal rulebase nil environment emit)
  :explicit
  (%io-newline-goal
   rulebase
   (%io-stream-entry rulebase stream :output environment operation)
   environment emit))

(defun %io-newline-goal (rulebase stream environment emit)
  (%io-newline rulebase stream)
  (funcall emit environment))

(defun %io-newline (rulebase entry)
  (terpri (prolog-stream-stream
           (or entry (prolog-io-context-current-output (%io-context rulebase))))))
