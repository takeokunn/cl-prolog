;;;; Precedence-climbing grammar parser for conventional Prolog source
;;;; text, built on top of the token stream produced by lexer.lisp, plus
;;;; the public read-prolog-term/read-prolog-clause/parse-prolog API.

(in-package #:cl-prolog)

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
         ;; A "-" immediately preceding a number is that number's negative
         ;; sign, not the prefix minus operator applied to a positive
         ;; number: this keeps a written negative number's canonical text
         ;; reading back as an atomic negative number rather than the
         ;; compound term (- N).
         (if (and (string= (%token-value token) "-")
                  (eq :number (%token-kind (%current-token parser))))
             (prog1 (- (%token-value (%current-token parser)))
               (incf (%parser-position parser)))
             (list (%operator-symbol (%token-value token)
                                     (%token-position token))
                   (%parse-expression
                    parser variables
                    (if (eq :fx (operator-definition-specifier definition))
                        (1+ binding-power)
                        binding-power))))))
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

(defun %non-associative-conflict-p (parser definition specifier)
  "True when SPECIFIER is :XFX and the next token is a binary operator sharing
DEFINITION's priority -- ISO forbids chaining same-priority non-associative
operators without parentheses."
  (and (eq specifier :xfx)
       (let ((next (%binary-operator-definition parser (%current-token parser))))
         (and next
              (= (operator-definition-priority definition)
                 (operator-definition-priority next))))))

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
                    (when (%non-associative-conflict-p parser definition specifier)
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
