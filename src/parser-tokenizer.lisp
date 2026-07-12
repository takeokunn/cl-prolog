(in-package #:cl-prolog)

(define-condition prolog-parse-error (error)
  ((description :initarg :description :reader prolog-parse-error-description))
  (:report (lambda (condition stream)
             (write-string (prolog-parse-error-description condition) stream)))
  (:documentation "A syntax error detected while reading Prolog source text."))

(defun %parse-error (control &rest arguments)
  (error 'prolog-parse-error
         :description (apply #'format nil control arguments)))

(defstruct (%token (:constructor %token (kind &optional value))) kind value)

(defvar *active-char-conversions* nil
  "Hash table of char_conversion/2 mappings applied while tokenizing.

NIL disables conversion.  Callers with a rulebase bind this around parsing
when the char_conversion flag is on; quoted tokens are never converted, as
ISO requires.")

(defun %operator-lexeme (definition)
  (string-downcase (symbol-name (operator-definition-name definition))))

(defun %word-operator-lexeme-p (lexeme)
  (and (plusp (length lexeme))
       (lower-case-p (char lexeme 0))
       (every #'%identifier-character-p lexeme)))

(defun %operator-lexemes (table &optional word-p)
  (remove-duplicates
   (loop for definition in (%operator-table-current table)
         for lexeme = (%operator-lexeme definition)
         when (eq word-p (%word-operator-lexeme-p lexeme))
           collect lexeme)
   :test #'string=))

(defun %compute-symbolic-token-lexemes (table)
  (coerce
   (sort
    (coerce (remove-duplicates
             (append (%operator-lexemes table nil)
                     (list "(" ")" "[" "]" "{" "}" "." "|"))
             :test #'string=)
            'vector)
    #'> :key #'length)
   'list))

(defvar *operator-lexeme-cache* (make-hash-table :test #'eq))

(defun %operator-table-lexemes (table)
  "Return cached word and symbolic lexemes for immutable TABLE."
  (or (gethash table *operator-lexeme-cache*)
      (setf (gethash table *operator-lexeme-cache*)
            (cons (%operator-lexemes table t)
                  (%compute-symbolic-token-lexemes table)))))

(defun %standard-operator-lexemes (&optional word-p)
  (if word-p
      (car (%operator-table-lexemes *standard-operator-table*))
      (%operator-lexemes *standard-operator-table*)))

(defun %symbolic-token-lexemes (&optional (table *standard-operator-table*))
  (if (eq table *standard-operator-table*)
      (cdr (%operator-table-lexemes table))
      (%compute-symbolic-token-lexemes table)))

(defun %tokenize-prolog (source &optional (operator-table *standard-operator-table*))
  (let* ((text (%prolog-source-string source))
         (length (length text))
         (position 0)
         (operator-lexemes (%operator-table-lexemes operator-table))
         (word-operators (car operator-lexemes))
         (symbolic-tokens (cdr operator-lexemes))
         (conversions *active-char-conversions*)
         (raw-mode nil)
         (tokens '()))
    (labels ((peek (&optional (offset 0))
               (let ((index (+ position offset)))
                 (when (< index length)
                   (let ((character (char text index)))
                     (if (and conversions (not raw-mode))
                         (or (gethash character conversions) character)
                         character)))))
             (take () (prog1 (peek) (incf position)))
             (emit (kind &optional value) (push (%token kind value) tokens))
             (skip-line ()
               (loop while (and (peek) (not (char= (peek) #\Newline))) do (take)))
             (skip-block ()
               (incf position 2)
               (loop until (and (peek) (peek 1)
                                (char= (peek) #\*) (char= (peek 1) #\/))
                     do (unless (peek) (%parse-error "Unterminated Prolog block comment."))
                        (take))
               (incf position 2))
             (scan-name ()
               (with-output-to-string (out)
                 (loop while (and (peek) (%identifier-character-p (peek)))
                       do (write-char (take) out))))
             (scan-quoted ()
               (take)
               (setf raw-mode t)
               (unwind-protect
                    (with-output-to-string (out)
                      (loop
                        (unless (peek) (%parse-error "Unterminated quoted Prolog atom."))
                        (let ((character (take)))
                          (cond
                            ((and (char= character #\') (peek) (char= (peek) #\'))
                             (take) (write-char #\' out))
                            ((char= character #\') (return))
                            ((and (char= character #\\) (peek)) (write-char (take) out))
                            (t (write-char character out))))))
                 (setf raw-mode nil)))
             (scan-number ()
               (let ((start position))
                 (loop while (and (peek) (digit-char-p (peek))) do (take))
                 (let ((float-p nil))
                 (when (and (peek) (peek 1) (char= (peek) #\.)
                            (digit-char-p (peek 1)))
                   (setf float-p t)
                   (take)
                   (loop while (and (peek) (digit-char-p (peek))) do (take)))
                 (when (and (peek) (member (peek) '(#\e #\E)))
                   (setf float-p t)
                   (take)
                   (when (and (peek) (member (peek) '(#\+ #\-))) (take))
                   (unless (and (peek) (digit-char-p (peek)))
                     (%parse-error "Malformed Prolog exponent."))
                   (loop while (and (peek) (digit-char-p (peek))) do (take)))
                 (let ((lexeme (subseq text start position)))
                   (if float-p
                       (let ((*read-default-float-format* 'double-float))
                         (read-from-string lexeme))
                       (parse-integer lexeme)))))))
      (loop while (peek) do
        (cond
          ((member (peek) '(#\Space #\Tab #\Return #\Newline)) (take))
          ((char= (peek) #\%) (skip-line))
          ((and (char= (peek) #\/) (peek 1) (char= (peek 1) #\*)) (skip-block))
          ((char= (peek) #\') (emit :quoted-atom (scan-quoted)))
          ((digit-char-p (peek)) (emit :number (scan-number)))
          ((or (upper-case-p (peek)) (char= (peek) #\_)) (emit :variable (scan-name)))
          ((lower-case-p (peek))
           (let ((name (scan-name)))
             (if (member name word-operators :test #'string=)
                 (emit :operator name)
                 (emit :atom name))))
          (t
           (let ((operator
                   (find-if (lambda (candidate)
                              (and (<= (+ position (length candidate)) length)
                                   (loop for index from 0 below (length candidate)
                                         always (eql (char candidate index)
                                                     (peek index)))))
                            symbolic-tokens)))
             (if operator
                 (progn
                   (incf position (length operator))
                   (emit :operator operator))
                 (let ((character (take)))
                   (if (char= character #\!)
                       (emit :atom "!")
                       (%parse-error "Unexpected Prolog character ~S." character))))))))
      (emit :eof)
      (coerce (nreverse tokens) 'vector))))
