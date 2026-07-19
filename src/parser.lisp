;;;; Lexer and precedence parser for conventional Prolog source text.

(in-package #:cl-prolog)

(progn
  (define-condition prolog-parse-error (error)
    ((description :initarg :description :reader prolog-parse-error-description))
    (:report (lambda (condition stream)
               (write-string (prolog-parse-error-description condition) stream)))
    (:documentation "A syntax error detected while reading Prolog source text."))

  (define-condition prolog-parser-resource-error (error)
    ((resource :initarg :resource
               :reader prolog-parser-resource-error-resource)
     (limit :initarg :limit
            :reader prolog-parser-resource-error-limit)
     (observed :initarg :observed
               :reader prolog-parser-resource-error-observed)
     (position :initarg :position
               :reader prolog-parser-resource-error-position))
    (:report
     (lambda (condition stream)
       (format stream
               "Prolog parser resource ~A exceeded limit ~D at position ~D (observed ~D)."
               (prolog-parser-resource-error-resource condition)
               (prolog-parser-resource-error-limit condition)
               (prolog-parser-resource-error-position condition)
               (prolog-parser-resource-error-observed condition))))
    (:documentation
     "A finite parser resource limit was exceeded while reading Prolog text."))

  (defparameter *max-prolog-source-characters* 1048576)
  (defparameter *max-prolog-delimiter-depth* 256)
  (defparameter *max-prolog-parser-depth* 256)
  (defparameter *max-prolog-tokens* 65536)
  (defparameter *max-prolog-identifier-length* 1024)
  (defparameter *max-prolog-quoted-lexeme-length* 65536)
  (defparameter *max-prolog-numeric-lexeme-length* 4096)
  (defparameter *max-prolog-interned-symbols* 65536)

  (defvar *parser-interned-symbols* (make-hash-table :test #'equal))

  (defun %parser-resource-error (resource limit observed position)
    (error 'prolog-parser-resource-error
           :resource resource
           :limit limit
           :observed observed
           :position position))

  (defun %check-parser-limit (resource limit observed position)
    (when (and limit (> observed limit))
      (%parser-resource-error resource limit observed position)))

  (defun %reserve-parser-symbol (name package position)
    (when *max-prolog-interned-symbols*
      (let ((key (cons (package-name package) name)))
        (unless (gethash key *parser-interned-symbols*)
          (%check-parser-limit "INTERNED_SYMBOLS"
                               *max-prolog-interned-symbols*
                               (1+ (hash-table-count
                                    *parser-interned-symbols*))
                               position)
          (setf (gethash key *parser-interned-symbols*) t)))))

  (defun %intern-parser-symbol (name package &optional (position 0))
    (multiple-value-bind (symbol status) (find-symbol name package)
      (if status
          symbol
          (progn
            (%reserve-parser-symbol name package position)
            (intern name package))))))

(defun %parse-error (control &rest arguments)
  (error 'prolog-parse-error
         :description (apply #'format nil control arguments)))

(defstruct (%token (:constructor %token (kind &optional value position)))
  kind
  value
  (position 0))
(defstruct (%parser (:constructor %parser (tokens operator-table)))
  tokens
  operator-table
  (position 0)
  (depth 0))

(defvar *parsing-dcg-body-p* nil)

(defvar *active-char-conversions* nil
  "Hash table of char_conversion/2 mappings applied while tokenizing.

NIL disables conversion.  Callers with a rulebase bind this around parsing
when the char_conversion flag is on; quoted tokens are never converted, as
ISO requires.")

(defun %read-prolog-term-source (stream)
  "Read through one top-level term terminator without consuming the next term."
  (let ((delimiters '())
        (state :code)
        (previous nil)
        (position 0))
    (labels ((record-character (character out)
               (incf position)
               (%check-parser-limit "SOURCE_CHARACTERS"
                                    *max-prolog-source-characters*
                                    position
                                    position)
               (write-char character out))
             (expected-closer (character)
               (cdr (assoc character
                           '((#\( . #\)) (#\[ . #\]) (#\{ . #\}))
                           :test #'char=)))
             (open-delimiter (character)
               (%check-parser-limit "DELIMITER_DEPTH"
                                    *max-prolog-delimiter-depth*
                                    (1+ (length delimiters))
                                    position)
               (push (expected-closer character) delimiters))
             (close-delimiter (character)
               (unless delimiters
                 (%parse-error
                  "Unexpected closing Prolog delimiter ~C at source position ~D."
                  character position))
               (unless (char= character (first delimiters))
                 (%parse-error
                  "Mismatched Prolog delimiter ~C at source position ~D; expected ~C."
                  character position (first delimiters)))
               (pop delimiters)))
      (with-output-to-string (out)
        (loop for character = (read-char stream nil nil)
              do (unless character
                   (unless (member state '(:code :line-comment))
                     (%parse-error
                      "Unexpected end of Prolog input while reading ~A."
                      state))
                   (when delimiters
                     (%parse-error
                      "Unexpected end of Prolog input; expected closing delimiter ~C."
                      (first delimiters)))
                   (return))
                 (record-character character out)
                 (ecase state
                   (:code
                    (cond
                      ((char= character #\') (setf state :quoted))
                      ((char= character #\%) (setf state :line-comment))
                      ((and (char= character #\/)
                            (eql (peek-char nil stream nil nil) #\*))
                       (record-character (read-char stream) out)
                       (setf state :block-comment))
                      ((member character '(#\( #\[ #\{))
                       (open-delimiter character))
                      ((member character '(#\) #\] #\}))
                       (close-delimiter character))
                      ((and (null delimiters)
                            (char= character #\.)
                            (not (and previous
                                      (digit-char-p previous)
                                      (let ((next
                                              (peek-char nil stream nil nil)))
                                        (and next (digit-char-p next))))))
                       (return))))
                   (:quoted
                    (cond
                      ((char= character #\\)
                       (setf state :quoted-escape))
                      ((char= character #\')
                       (if (eql (peek-char nil stream nil nil) #\')
                           (progn
                             (record-character (read-char stream) out)
                             (setf previous #\'))
                           (setf state :code)))))
                   (:quoted-escape (setf state :quoted))
                   (:line-comment
                    (when (char= character #\Newline)
                      (setf state :code)))
                   (:block-comment
                    (when (and (char= character #\*)
                               (eql (peek-char nil stream nil nil) #\/))
                      (record-character (read-char stream) out)
                      (setf state :code))))
                 (setf previous character))))))

(defun %prolog-source-string (source)
  (etypecase source
    (string
     (%check-parser-limit "SOURCE_CHARACTERS"
                          *max-prolog-source-characters*
                          (length source)
                          (length source))
     source)
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
  "Cache lexemes only for the immutable standard operator table."
  (if (eq table *standard-operator-table*)
      (progn
        (when (> (hash-table-count *operator-lexeme-cache*) 1)
          (clrhash *operator-lexeme-cache*))
        (or (gethash table *operator-lexeme-cache*)
            (setf (gethash table *operator-lexeme-cache*)
                  (cons (%operator-lexemes table t)
                        (%compute-symbolic-token-lexemes table)))))
      (cons (%operator-lexemes table t)
            (%compute-symbolic-token-lexemes table))))

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
         (token-count 0)
         (delimiters '())
         (tokens '()))
    (labels ((peek (&optional (offset 0))
               (let ((index (+ position offset)))
                 (when (< index length)
                   (let ((character (char text index)))
                     (if (and conversions (not raw-mode))
                         (or (gethash character conversions) character)
                         character)))))
             (take ()
               (prog1 (peek) (incf position)))
             (expected-closer (operator)
               (cond ((string= operator "(") ")")
                     ((string= operator "[") "]")
                     ((string= operator "{") "}")))
             (note-delimiter (operator start)
               (let ((closer (expected-closer operator)))
                 (cond
                   (closer
                    (%check-parser-limit "DELIMITER_DEPTH"
                                         *max-prolog-delimiter-depth*
                                         (1+ (length delimiters))
                                         start)
                    (push closer delimiters))
                   ((member operator '(")" "]" "}") :test #'string=)
                    (unless delimiters
                      (%parse-error
                       "Unexpected closing Prolog delimiter ~A at source position ~D."
                       operator start))
                    (unless (string= operator (first delimiters))
                      (%parse-error
                       "Mismatched Prolog delimiter ~A at source position ~D; expected ~A."
                       operator start (first delimiters)))
                    (pop delimiters)))))
             (emit (kind &optional value (start position))
               (unless (eq kind :eof)
                 (%check-parser-limit "TOKEN_COUNT"
                                      *max-prolog-tokens*
                                      (1+ token-count)
                                      start)
                 (incf token-count))
               (when (eq kind :operator)
                 (note-delimiter value start))
               (push (%token kind value start) tokens))
             (skip-line ()
               (loop while (and (peek) (not (char= (peek) #\Newline)))
                     do (take)))
             (skip-block ()
               (incf position 2)
               (loop until (and (peek) (peek 1)
                                (char= (peek) #\*)
                                (char= (peek 1) #\/))
                     do (unless (peek)
                          (%parse-error "Unterminated Prolog block comment."))
                        (take))
               (incf position 2))
             (scan-name ()
               (let ((start position))
                 (with-output-to-string (out)
                   (loop while (and (peek) (%identifier-character-p (peek)))
                         do (%check-parser-limit
                             "IDENTIFIER_LENGTH"
                             *max-prolog-identifier-length*
                             (1+ (- position start))
                             position)
                            (write-char (take) out)))))
             (scan-quoted ()
               (take)
               (setf raw-mode t)
               (unwind-protect
                    (let ((content-length 0))
                      (labels ((write-content (character out)
                                 (incf content-length)
                                 (%check-parser-limit
                                  "QUOTED_LEXEME_LENGTH"
                                  *max-prolog-quoted-lexeme-length*
                                  content-length
                                  position)
                                 (write-char character out)))
                        (with-output-to-string (out)
                          (loop
                            (unless (peek)
                              (%parse-error "Unterminated quoted Prolog atom."))
                            (let ((character (take)))
                              (cond
                                ((and (char= character #\')
                                      (peek)
                                      (char= (peek) #\'))
                                 (take)
                                 (write-content #\' out))
                                ((char= character #\')
                                 (return))
                                ((and (char= character #\\) (peek))
                                 (write-content (take) out))
                                (t
                                 (write-content character out))))))))
                 (setf raw-mode nil)))
             (scan-number ()
               (let ((start position))
                 (labels ((take-number-character ()
                            (%check-parser-limit
                             "NUMERIC_LEXEME_LENGTH"
                             *max-prolog-numeric-lexeme-length*
                             (1+ (- position start))
                             position)
                            (take)))
                   (loop while (and (peek) (digit-char-p (peek)))
                         do (take-number-character))
                   (let ((float-p nil))
                     (when (and (peek) (peek 1)
                                (char= (peek) #\.)
                                (digit-char-p (peek 1)))
                       (setf float-p t)
                       (take-number-character)
                       (loop while (and (peek) (digit-char-p (peek)))
                             do (take-number-character)))
                     (when (and (peek) (member (peek) '(#\e #\E)))
                       (setf float-p t)
                       (take-number-character)
                       (when (and (peek) (member (peek) '(#\+ #\-)))
                         (take-number-character))
                       (unless (and (peek) (digit-char-p (peek)))
                         (%parse-error "Malformed Prolog exponent."))
                       (loop while (and (peek) (digit-char-p (peek)))
                             do (take-number-character)))
                     (let ((lexeme (subseq text start position)))
                       (if float-p
                           (let ((*read-default-float-format* 'double-float))
                             (read-from-string lexeme))
                           (parse-integer lexeme))))))))
      (loop while (peek)
            do (let ((start position))
                 (cond
                   ((member (peek) '(#\Space #\Tab #\Return #\Newline))
                    (take))
                   ((char= (peek) #\%)
                    (skip-line))
                   ((and (char= (peek) #\/)
                         (peek 1)
                         (char= (peek 1) #\*))
                    (skip-block))
                   ((char= (peek) #\')
                    (emit :quoted-atom (scan-quoted) start))
                   ((digit-char-p (peek))
                    (emit :number (scan-number) start))
                   ((or (upper-case-p (peek)) (char= (peek) #\_))
                    (emit :variable (scan-name) start))
                   ((lower-case-p (peek))
                    (let ((name (scan-name)))
                      (if (member name word-operators :test #'string=)
                          (emit :operator name start)
                          (emit :atom name start))))
                   (t
                    (let ((operator
                            (find-if
                             (lambda (candidate)
                               (and (<= (+ position (length candidate)) length)
                                    (loop for index from 0 below (length candidate)
                                          always (eql (char candidate index)
                                                      (peek index)))))
                             symbolic-tokens)))
                      (if operator
                          (progn
                            (incf position (length operator))
                            (emit :operator operator start))
                          (let ((character (take)))
                            (if (char= character #\!)
                                (emit :atom "!" start)
                                (%parse-error
                                 "Unexpected Prolog character ~S at source position ~D."
                                 character start)))))))))
      (when delimiters
        (%parse-error
         "Unexpected end of Prolog input; expected closing delimiter ~A."
         (first delimiters)))
      (emit :eof nil position)
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

(defun %prolog-symbol (name &key preserve-case (position 0) track-resource-p)
  (let ((canonical-name (if preserve-case name (string-upcase name)))
        (package (find-package '#:cl-prolog)))
    (if track-resource-p
        (%intern-parser-symbol canonical-name package position)
        (intern canonical-name package))))

(defun %prolog-atom-symbol
    (name &key preserve-case (position 0) track-resource-p)
  (let* ((canonical-name (if preserve-case name (string-upcase name)))
         (package (find-package '#:cl-prolog)))
    (flet ((intern-name (symbol-name target-package)
             (if track-resource-p
                 (%intern-parser-symbol symbol-name target-package position)
                 (intern symbol-name target-package))))
      (multiple-value-bind (symbol status) (find-symbol canonical-name package)
        (if (or (eq status :inherited)
                (and (plusp (length canonical-name))
                     (char= (char canonical-name 0) #\?)))
            (intern-name canonical-name
                         (find-package '#:cl-prolog.user-atoms))
            (or symbol
                (intern-name canonical-name package)))))))

(defun %variable-symbol (name variables &key (position 0))
  (if (string= name "_")
      (fresh-logic-variable "?ANON")
      (or (gethash name variables)
          (setf (gethash name variables)
                (%intern-parser-symbol
                 (concatenate 'string "?" (string-upcase name))
                 (find-package '#:cl-prolog)
                 position)))))

(defun %operator-definition-for-token (parser token specifiers)
  (when (eq :operator (%token-kind token))
    (find-if
     (lambda (definition)
       (and (member (operator-definition-specifier definition)
                    specifiers
                    :test #'eq)
            (string= (%token-value token)
                     (%operator-lexeme definition))))
     (%operator-table-current (%parser-operator-table parser)))))

(defun %operator-binding-power (definition)
  (- +maximum-operator-priority+
     (operator-definition-priority definition)))

(defun %binary-operator-definition (parser token)
  (%operator-definition-for-token parser token '(:xfx :xfy :yfx)))

(defun %prefix-operator-definition (parser token)
  (%operator-definition-for-token parser token '(:fx :fy)))

(defun %operator-symbol (operator &optional (position 0))
  (cond ((string= operator ",") 'and)
        ((string= operator ";") 'or)
        ((string= operator "\\+") 'not)
        (t (%prolog-symbol operator
                           :position position
                           :track-resource-p t))))

(defun %normalize-control-expression (operator left right &optional (position 0))
  (cond
    ((and (string= operator ";") (consp left) (eq (first left) '->))
     (list 'if-then-else (second left) (third left) right))
    ((and (string= operator ";") (consp left) (eq (first left) '*->))
     (list 'soft-if-then-else (second left) (third left) right))
    (t (list (%operator-symbol operator position) left right))))

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
         (list (%operator-symbol (%token-value token)
                                 (%token-position token))
               (%parse-expression
                parser variables
                (if (eq :fx (operator-definition-specifier definition))
                    (1+ binding-power)
                    binding-power)))))
      ((eq :number (%token-kind token))
       (incf (%parser-position parser))
       (%token-value token))
      ((eq :variable (%token-kind token))
       (incf (%parser-position parser))
       (%variable-symbol (%token-value token)
                         variables
                         :position (%token-position token)))
      ((member (%token-kind token) '(:atom :quoted-atom))
       (incf (%parser-position parser))
       (let ((atom (%prolog-atom-symbol
                    (%token-value token)
                    :preserve-case (eq :quoted-atom (%token-kind token))
                    :position (%token-position token)
                    :track-resource-p t)))
         (if (%accept-token parser :operator "(")
             (let ((arguments '()))
               (unless (%accept-token parser :operator ")")
                 (loop (push (let ((*parsing-dcg-body-p* nil))
                               (%parse-expression parser variables 201))
                             arguments)
                       (cond ((%accept-token parser :operator ","))
                             (t
                              (%expect-token parser :operator ")")
                              (return)))))
               (cons atom (nreverse arguments)))
             atom)))
      (t
       (%parse-error "Expected a Prolog term, got ~S." token)))))

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
  (let ((depth (1+ (%parser-depth parser))))
    (%check-parser-limit "PARSER_DEPTH"
                         *max-prolog-parser-depth*
                         depth
                         (%token-position (%current-token parser)))
    (incf (%parser-depth parser))
    (unwind-protect
         (let ((left (%parse-primary parser variables minimum-precedence)))
           (loop for token = (%current-token parser)
                 for definition = (%binary-operator-definition parser token)
                 for precedence = (and definition
                                       (%operator-binding-power definition))
                 while (and precedence (>= precedence minimum-precedence))
                 for operator = (%token-value token)
                 for specifier = (operator-definition-specifier definition)
                 do (incf (%parser-position parser))
                    (setf left
                          (%normalize-control-expression
                           operator
                           left
                           (let ((*parsing-dcg-body-p*
                                   (or *parsing-dcg-body-p*
                                       (string= operator "-->"))))
                             (%parse-expression
                              parser
                              variables
                              (if (eq specifier :xfy)
                                  precedence
                                  (1+ precedence))))
                           (%token-position token)))
                    (when
                        (and
                         (eq specifier :xfx)
                         (let ((next
                                 (%binary-operator-definition
                                  parser
                                  (%current-token parser))))
                           (and next
                                (= (operator-definition-priority definition)
                                   (operator-definition-priority next)))))
                      (%parse-error
                       "Non-associative Prolog operator ~A cannot be chained."
                       operator))
                 finally (return left)))
      (decf (%parser-depth parser)))))

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
