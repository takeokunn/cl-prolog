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

(define-builtin (get_code code) (rulebase environment depth emit)
  (declare (ignore depth))
  (%unify-emit code
               (%io-read-code
                (prolog-io-context-current-input (%io-context rulebase))
                environment (%io-operation "GET_CODE"))
               environment emit))

(define-builtin (get_code stream code) (rulebase environment depth emit)
  (declare (ignore depth))
  (let ((operation (%io-operation "GET_CODE")))
    (%unify-emit code
                 (%io-read-code
                  (%io-stream-entry rulebase stream :input environment operation)
                  environment operation)
                 environment emit)))

(define-builtin (peek_code code) (rulebase environment depth emit)
  (declare (ignore depth))
  (%unify-emit
   code
   (%io-read-code
    (prolog-io-context-current-input (%io-context rulebase))
    environment (%io-operation "PEEK_CODE") :peek t)
   environment emit))

(define-builtin (peek_code stream code) (rulebase environment depth emit)
  (declare (ignore depth))
  (let ((operation (%io-operation "PEEK_CODE")))
    (%unify-emit
     code
     (%io-read-code
      (%io-stream-entry rulebase stream :input environment operation)
      environment operation :peek t)
     environment emit)))

(define-builtin (put_code code) (rulebase environment depth emit)
  (declare (ignore depth))
  (%io-write-code
   (prolog-io-context-current-output (%io-context rulebase))
   code environment (%io-operation "PUT_CODE"))
  (funcall emit environment))

(define-builtin (put_code stream code) (rulebase environment depth emit)
  (declare (ignore depth))
  (let ((operation (%io-operation "PUT_CODE")))
    (%io-write-code
     (%io-stream-entry rulebase stream :output environment operation)
     code environment operation)
    (funcall emit environment)))
