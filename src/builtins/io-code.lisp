;;;; ISO character-code stream predicates.

(in-package #:cl-prolog)

(defun %io-read-code (entry environment operation &key peek)
  (%io-require-stream-type entry :text environment operation)
  (let ((character (if peek
                       (peek-char nil (prolog-stream-stream entry) nil nil)
                       (read-char (prolog-stream-stream entry) nil nil))))
    (if character (char-code character) -1)))

(defun %io-code-character (term environment operation)
  (let ((value (%io-resolve-term term environment operation)))
    (unless (integerp value)
      (%raise-type-error "INTEGER" value environment operation
                         "character code must be an integer"))
    (or (and (<= 0 value)
             (< value char-code-limit)
             (code-char value))
        (%raise-domain-error
         "CHARACTER_CODE" value environment operation
         "integer does not designate a supported character"))))

(defun %io-write-code (entry term environment operation)
  (%io-require-stream-type entry :text environment operation)
  (write-char (%io-code-character term environment operation)
              (prolog-stream-stream entry)))

(%define-io-dual-builtin
    (get_code (code) (code) "GET_CODE")
    (rulebase environment depth emit)
  :current (%unify-emit code
                        (%io-read-code
                         (%io-current-input-entry rulebase)
                         environment operation)
                        environment emit)
  :explicit (%unify-emit code
                         (%io-read-code
                          (%io-stream-entry rulebase stream :input environment operation)
                          environment operation)
                         environment emit))

(%define-io-dual-builtin
    (peek_code (code) (code) "PEEK_CODE")
    (rulebase environment depth emit)
  :current (%unify-emit
            code
            (%io-read-code
             (%io-current-input-entry rulebase)
             environment operation :peek t)
            environment emit)
  :explicit (%unify-emit
             code
             (%io-read-code
              (%io-stream-entry rulebase stream :input environment operation)
              environment operation :peek t)
             environment emit))

(%define-io-dual-builtin
    (put_code (code) (code) "PUT_CODE")
    (rulebase environment depth emit)
  :current (progn
             (%io-write-code
              (%io-current-output-entry rulebase)
              code environment operation)
             (funcall emit environment))
  :explicit (progn
              (%io-write-code
               (%io-stream-entry rulebase stream :output environment operation)
               code environment operation)
              (funcall emit environment)))
