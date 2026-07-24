;;;; Lexer resource limits and shared representations: the parse-error and
;;;; parser-resource-error conditions, finite resource-limit parameters and
;;;; checks, symbol interning under those limits, and the %token/%parser
;;;; structs the tokenizer (lexer-tokenizer.lisp) builds on. Operator-lexeme
;;;; table computation lives in lexer-operator-lexemes.lisp.

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
