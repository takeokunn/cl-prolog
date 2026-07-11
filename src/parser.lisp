;;;; Lexer and precedence parser for conventional Prolog source text.

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
(defstruct (%parser (:constructor %parser (tokens operator-table)))
  tokens
  operator-table
  (position 0))

(defvar *parsing-dcg-body-p* nil)

(defun %read-prolog-term-source (stream)
  "Read through one top-level term terminator without consuming the next term."
  (let ((depth 0)
        (state :code)
        (previous nil))
    (with-output-to-string (out)
      (loop for character = (read-char stream nil nil)
            do (unless character
                 (if (member state '(:code :line-comment))
                     (return)
                     (%parse-error "Unexpected end of Prolog input while reading ~A."
                                   state)))
               (write-char character out)
               (ecase state
                 (:code
                  (cond
                    ((char= character #\') (setf state :quoted))
                    ((char= character #\%) (setf state :line-comment))
                    ((and (char= character #\/)
                          (eql (peek-char nil stream nil nil) #\*))
                     (write-char (read-char stream) out)
                     (setf state :block-comment))
                    ((member character '(#\( #\[)) (incf depth))
                    ((member character '(#\) #\])) (decf depth))
                    ((and (zerop depth)
                          (char= character #\.)
                          (not (and previous
                                    (digit-char-p previous)
                                    (let ((next (peek-char nil stream nil nil)))
                                      (and next (digit-char-p next))))))
                     (return))))
                 (:quoted
                  (cond
                    ((char= character #\\) (setf state :quoted-escape))
                    ((char= character #\')
                     (if (eql (peek-char nil stream nil nil) #\')
                         (progn
                           (write-char (read-char stream) out)
                           (setf previous #\'))
                         (setf state :code)))))
                 (:quoted-escape (setf state :quoted))
                 (:line-comment
                  (when (char= character #\Newline) (setf state :code)))
                 (:block-comment
                  (when (and (char= character #\*)
                             (eql (peek-char nil stream nil nil) #\/))
                    (write-char (read-char stream) out)
                    (setf state :code))))
               (setf previous character)))))

(defun %prolog-source-string (source)
  (etypecase source
    (string source)
    (stream (%read-prolog-term-source source))))

(defun %identifier-character-p (character)
  (or (alphanumericp character) (char= character #\_)))

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
         (tokens '()))
    (labels ((peek (&optional (offset 0))
               (let ((index (+ position offset)))
                 (and (< index length) (char text index))))
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
               (let ((start position))
                 (loop while (and (peek) (%identifier-character-p (peek))) do (take))
                 (subseq text start position)))
             (scan-quoted ()
               (take)
               (with-output-to-string (out)
                 (loop
                   (unless (peek) (%parse-error "Unterminated quoted Prolog atom."))
                   (let ((character (take)))
                     (cond
                       ((and (char= character #\') (peek) (char= (peek) #\'))
                        (take) (write-char #\' out))
                       ((char= character #\') (return))
                       ((and (char= character #\\) (peek)) (write-char (take) out))
                       (t (write-char character out)))))))
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
                                   (string= candidate text :start2 position
                                                        :end2 (+ position (length candidate)))))
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

(defun %current-token (parser)
  (aref (%parser-tokens parser) (%parser-position parser)))

(defun %accept-token (parser kind &optional value)
  (let ((token (%current-token parser)))
    (when (and (eq kind (%token-kind token))
               (or (null value) (equal value (%token-value token))))
      (incf (%parser-position parser))
      token)))

(defun %expect-token (parser kind &optional value)
  (or (%accept-token parser kind value)
      (%parse-error "Expected Prolog token ~S~@[ ~S~], got ~S."
                    kind value (%current-token parser))))

(defun %prolog-symbol (name &key preserve-case)
  (intern (if preserve-case name (string-upcase name))
          (find-package '#:cl-prolog)))

(defun %prolog-atom-symbol (name &key preserve-case)
  (let* ((canonical-name (if preserve-case name (string-upcase name)))
         (package (find-package '#:cl-prolog)))
    (multiple-value-bind (symbol status) (find-symbol canonical-name package)
      (if (eq status :inherited)
          (intern canonical-name (find-package '#:cl-prolog.user-atoms))
          (or symbol (intern canonical-name package))))))

(defun %variable-symbol (name variables)
  (if (string= name "_")
      (fresh-logic-variable "?ANON")
      (or (gethash name variables)
          (setf (gethash name variables)
                (intern (concatenate 'string "?" (string-upcase name))
                        (find-package '#:cl-prolog))))))

(defun %operator-definition-for-token (parser token specifiers)
  (when (eq :operator (%token-kind token))
    (let ((name (%prolog-symbol (%token-value token))))
      (find-if (lambda (definition)
                 (member (operator-definition-specifier definition)
                         specifiers :test #'eq))
               (%operator-table-find (%parser-operator-table parser) name)))))

(defun %operator-binding-power (definition)
  (- +maximum-operator-priority+
     (operator-definition-priority definition)))

(defun %binary-operator-definition (parser token)
  (%operator-definition-for-token parser token '(:xfx :xfy :yfx)))

(defun %prefix-operator-definition (parser token)
  (%operator-definition-for-token parser token '(:fx :fy)))

(defun %operator-symbol (operator)
  (cond ((string= operator ",") 'and)
        ((string= operator ";") 'or)
        ((string= operator "\\+") 'not)
        (t (%prolog-symbol operator))))

(defun %normalize-control-expression (operator left right)
  (cond
    ((and (string= operator ";") (consp left) (eq (first left) '->))
     (list 'if-then-else (second left) (third left) right))
    ((and (string= operator ";") (consp left) (eq (first left) '*->))
     (list 'soft-if-then-else (second left) (third left) right))
    (t (list (%operator-symbol operator) left right))))

(declaim (ftype function %parse-expression %parse-list))

(defun %parse-primary (parser variables minimum-precedence)
  (let ((token (%current-token parser)))
    (cond
      ((%accept-token parser :operator "(")
       (prog1 (%parse-expression parser variables 0)
         (%expect-token parser :operator ")")))
      ((%accept-token parser :operator "[")
       (let ((list (%parse-list parser variables)))
         (if *parsing-dcg-body-p* (list 'dcg-terminals list) list)))
      ((%accept-token parser :operator "{")
       (prog1 (list 'brace (let ((*parsing-dcg-body-p* nil))
                            (%parse-expression parser variables 0)))
         (%expect-token parser :operator "}")))
      ((%prefix-operator-definition parser token)
       (let* ((definition (%prefix-operator-definition parser token))
              (binding-power (%operator-binding-power definition)))
         (when (< binding-power minimum-precedence)
           (%parse-error "Prefix Prolog operator ~A is not valid in this context."
                         (%token-value token)))
         (incf (%parser-position parser))
         (list (%operator-symbol (%token-value token))
               (%parse-expression
                parser variables
                (if (eq :fx (operator-definition-specifier definition))
                    (1+ binding-power)
                    binding-power)))))
      ((eq :number (%token-kind token))
       (incf (%parser-position parser)) (%token-value token))
      ((eq :variable (%token-kind token))
       (incf (%parser-position parser))
       (%variable-symbol (%token-value token) variables))
      ((member (%token-kind token) '(:atom :quoted-atom))
       (incf (%parser-position parser))
       (let ((atom (%prolog-atom-symbol
                    (%token-value token)
                    :preserve-case (eq :quoted-atom (%token-kind token)))))
         (if (%accept-token parser :operator "(")
             (let ((arguments '()))
               (unless (%accept-token parser :operator ")")
                 (loop (push (let ((*parsing-dcg-body-p* nil))
                               (%parse-expression parser variables 201))
                             arguments)
                       (cond ((%accept-token parser :operator ","))
                             (t (%expect-token parser :operator ")") (return)))))
               (cons atom (nreverse arguments)))
             atom)))
      (t (%parse-error "Expected a Prolog term, got ~S." token)))))

(defun %parse-list (parser variables)
  (when (%accept-token parser :operator "]") (return-from %parse-list '()))
  (let ((elements '()) (tail '()))
    (loop (push (%parse-expression parser variables 201) elements)
          (cond
            ((%accept-token parser :operator ","))
            ((%accept-token parser :operator "|")
             (setf tail (%parse-expression parser variables 201))
             (%expect-token parser :operator "]") (return))
            (t (%expect-token parser :operator "]") (return))))
    (reduce #'cons (nreverse elements) :from-end t :initial-value tail)))

(defun %parse-expression (parser variables minimum-precedence)
  (let ((left (%parse-primary parser variables minimum-precedence)))
    (loop for token = (%current-token parser)
          for definition = (%binary-operator-definition parser token)
          for precedence = (and definition (%operator-binding-power definition))
          while (and precedence (>= precedence minimum-precedence))
          for operator = (%token-value token)
          for specifier = (operator-definition-specifier definition)
          do (incf (%parser-position parser))
             (setf left (%normalize-control-expression
                         operator left
                         (let ((*parsing-dcg-body-p*
                                 (or *parsing-dcg-body-p*
                                     (string= operator "-->"))))
                           (%parse-expression parser variables
                                              (if (eq specifier :xfy)
                                                   precedence
                                                   (1+ precedence))))))
             (when (and (eq specifier :xfx)
                        (let ((next (%binary-operator-definition
                                     parser
                                     (%current-token parser))))
                          (and next
                               (= (operator-definition-priority definition)
                                  (operator-definition-priority next)))))
               (%parse-error "Non-associative Prolog operator ~A cannot be chained."
                             operator))
          finally (return left))))

(defun %body-goals (body)
  (if (and (consp body) (eq (first body) 'and)) (rest body) (list body)))

(defun %parse-next-prolog-form (parser)
  (when (eq :eof (%token-kind (%current-token parser)))
    (return-from %parse-next-prolog-form (values nil :eof)))
  (let ((variables (make-hash-table :test #'equal)))
    (if (%accept-token parser :operator "?-")
        (let ((body (%parse-expression parser variables 0)))
          (%expect-token parser :operator ".")
          (values body :query))
        (let ((head (%parse-expression parser variables 201)))
          (if (%accept-token parser :operator ":-")
              (let ((body (%parse-expression parser variables 0)))
                (%expect-token parser :operator ".")
                (values (make-clause (if (symbolp head) (list head) head)
                                     (%body-goals body))
                        :clause))
              (progn
                (%expect-token parser :operator ".")
                (values (make-clause (if (symbolp head) (list head) head)) :clause)))))))

(defun read-prolog-term (source &optional (operator-table *standard-operator-table*))
  "Read one Prolog term from SOURCE, which may be a string or stream."
  (let* ((parser (%parser (%tokenize-prolog source operator-table) operator-table))
         (term (%parse-expression parser (make-hash-table :test #'equal) 0)))
    (%accept-token parser :operator ".")
    (%expect-token parser :eof)
    term))

(defun read-prolog-clause (source &optional (operator-table *standard-operator-table*))
  "Read one fact or rule from SOURCE and return a CLAUSE."
  (let ((parser (%parser (%tokenize-prolog source operator-table) operator-table)))
    (multiple-value-bind (form kind) (%parse-next-prolog-form parser)
      (unless (eq kind :clause) (%parse-error "Expected a Prolog clause."))
      (%expect-token parser :eof)
      form)))

(defun parse-prolog (source &optional (operator-table *standard-operator-table*))
  "Parse SOURCE into CLAUSE objects and untagged query goal forms in source order."
  (let ((parser (%parser (%tokenize-prolog source operator-table) operator-table))
        (forms '()))
    (loop (multiple-value-bind (form kind) (%parse-next-prolog-form parser)
            (when (eq kind :eof) (return (nreverse forms)))
            (push form forms)))))
