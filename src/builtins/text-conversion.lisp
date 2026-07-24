;;;; Shared text-conversion primitives for the atom/character/number
;;;; builtins in atom-ops.lisp and atom-number-conversion.lisp: resource
;;;; limits, character/code list conversion, and number-text parsing.

(in-package #:cl-prolog)

(progn
  (defparameter *max-prolog-derived-atom-candidates*
    *max-prolog-numeric-lexeme-length*
    "Maximum number of atom candidates a single relational builtin may derive.")

  (defun %check-resource-limit
      (actual limit resource environment operation message)
    (when (> actual limit)
      (%raise-resource-error resource environment operation message))
    actual)

  (defun %check-text-resource-limit
      (text limit resource environment operation message)
    (%check-resource-limit (length text) limit resource environment operation message)
    text)

  (defun %check-atom-text-limit (atom environment operation)
    (%check-text-resource-limit
     (%atom-text atom) *max-prolog-quoted-lexeme-length* "ATOM_LENGTH"
     environment operation "atom text exceeds the configured length limit"))

  (defun %integer-within-decimal-digit-limit-p (integer digit-limit)
    (let* ((magnitude (abs integer))
           (bit-limit (ceiling (* digit-limit 3322) 1000))
           (bits (integer-length magnitude)))
      (or (zerop magnitude)
          (< bits bit-limit)
          (and (= bits bit-limit)
               (< magnitude (expt 10 digit-limit))))))

  (defun %check-integer-text-limit (integer environment operation)
    (let ((digit-limit (- *max-prolog-numeric-lexeme-length*
                          (if (minusp integer) 1 0))))
      (unless (and (plusp digit-limit)
                   (%integer-within-decimal-digit-limit-p integer digit-limit))
        (%raise-resource-error
         "INTEGER_SIZE" environment operation
         "integer decimal representation exceeds the configured length limit")))
    integer)

  (defun %atom-text (atom)
    (symbol-name atom)))

(defun %text-atom (text &optional environment (operation (%iso-atom "ATOM")))
  (%check-text-resource-limit
   text *max-prolog-quoted-lexeme-length* "ATOM_LENGTH" environment operation
   "atom text exceeds the configured length limit")
  (%prolog-atom-symbol text :preserve-case t))

(defun %character-atom-p (term)
  (and (%term-atom-p term) (= 1 (length (%atom-text term)))))

(defun %ensure-atom-value (value environment operation argument)
  (when (logic-var-p value)
    (%raise-instantiation-error environment operation
                                (format nil "~A must be instantiated" argument)))
  (unless (%term-atom-p value)
    (%raise-type-error "ATOM" value environment operation
                       (format nil "~A must be an atom" argument)))
  value)

(defun %ensure-nonnegative-integer-or-variable (value environment operation argument)
  (unless (logic-var-p value)
    (unless (integerp value)
      (%raise-type-error "INTEGER" value environment operation
                         (format nil "~A must be an integer" argument)))
    (when (minusp value)
      (%raise-domain-error "NOT_LESS_THAN_ZERO" value environment operation
                           (format nil "~A must not be negative" argument))))
  value)

(defun %ensure-proper-instantiated-list
    (value environment operation argument
     &key element-checker
          (limit *max-prolog-quoted-lexeme-length*)
          (resource "LIST_LENGTH"))
  (let ((visited (make-hash-table :test #'eq))
        (tail value)
        (count 0))
    (loop
      (cond
        ((null tail) (return count))
        ((logic-var-p tail)
         (%raise-instantiation-error
          environment operation (format nil "~A must be instantiated" argument)))
        ((not (consp tail))
         (%raise-type-error "LIST" value environment operation
                            (format nil "~A must be a proper list" argument)))
        ((gethash tail visited)
         (%raise-type-error "LIST" value environment operation
                            (format nil "~A must be a finite proper list" argument)))
        (t
         (setf (gethash tail visited) t)
         (when (logic-var-p (car tail))
           (%raise-instantiation-error
            environment operation (format nil "~A must be instantiated" argument)))
         (when element-checker
           (funcall element-checker (car tail)))
         (incf count)
         (%check-resource-limit
          count limit resource environment operation
          "list exceeds the configured length limit")
         (setf tail (cdr tail)))))))

(defun %atom-character-list
    (atom environment operation
     &optional (limit *max-prolog-quoted-lexeme-length*) (resource "ATOM_LENGTH"))
  (let ((text (%atom-text atom)))
    (%check-text-resource-limit
     text limit resource environment operation
     "atom text exceeds the configured length limit")
    (map 'list (lambda (character)
                 (%text-atom (string character) environment operation))
         text)))

(defun %atom-code-list
    (atom environment operation
     &optional (limit *max-prolog-quoted-lexeme-length*) (resource "ATOM_LENGTH"))
  (let ((text (%atom-text atom)))
    (%check-text-resource-limit
     text limit resource environment operation
     "atom text exceeds the configured length limit")
    (map 'list #'char-code text)))

(defun %character-list-text
    (characters environment operation
     &optional (limit *max-prolog-quoted-lexeme-length*) (resource "LIST_LENGTH"))
  (let* ((count
           (%ensure-proper-instantiated-list
            characters environment operation "character list"
            :limit limit
            :resource resource
            :element-checker
            (lambda (character)
              (unless (%character-atom-p character)
                (%raise-type-error
                 "CHARACTER" character environment operation
                 "atom_chars/2 and number_chars/2 require character atoms")))))
         (text (make-string count)))
    (loop for character in characters
          for index from 0
          do (setf (char text index) (char (%atom-text character) 0)))
    text))

(defun %code-character (code environment operation)
  (unless (integerp code)
    (%raise-type-error "INTEGER" code environment operation
                       "character codes must be integers"))
  (unless (and (<= 0 code) (< code char-code-limit))
    (%raise-domain-error "CHARACTER_CODE" code environment operation
                         "integer is not a character code"))
  (or (code-char code)
      (%raise-domain-error "CHARACTER_CODE" code environment operation
                           "integer is not a character code")))

(defun %code-list-text
    (codes environment operation
     &optional (limit *max-prolog-quoted-lexeme-length*) (resource "LIST_LENGTH"))
  (let* ((count
           (%ensure-proper-instantiated-list
            codes environment operation "code list"
            :limit limit
            :resource resource
            :element-checker
            (lambda (code) (%code-character code environment operation))))
         (text (make-string count)))
    (loop for code in codes
          for index from 0
          do (setf (char text index)
                   (%code-character code environment operation)))
    text))

(defun %number-text (number environment operation)
  (cond
    ((integerp number)
     (%check-integer-text-limit number environment operation)
     (write-to-string number :base 10 :radix nil :readably t))
    ((floatp number)
     (let ((text (write-to-string number :base 10 :radix nil :readably t)))
       (%check-text-resource-limit
        text *max-prolog-numeric-lexeme-length* "NUMBER_TEXT_LENGTH"
        environment operation
        "numeric text exceeds the configured length limit")
       (map-into text
                 (lambda (character)
                   (if (find character "sSfFdDlL") #\e character))
                 text)
       ;; Reject implementation-specific non-finite and reader forms.
       (%text-number text environment operation)
       text))
    ((realp number)
     (%raise-domain-error "PROLOG_NUMBER" number environment operation
                          "ratios are not Prolog numeric terms"))
    (t
     (%raise-type-error "NUMBER" number environment operation
                        "first argument must be a Prolog integer or float"))))

(defun %raise-syntax-number-error (text environment operation)
  (let* ((length (length text))
         (culprit-text
           (if (<= length 64)
               text
               (format nil "~A...<~D characters>" (subseq text 0 32) length))))
    (%raise-domain-error "NUMBER_TEXT" (make-symbol culprit-text)
                         environment operation
                         "text is not a valid number")))

(defun %text-number (text environment operation)
  (%check-text-resource-limit
   text *max-prolog-numeric-lexeme-length* "NUMBER_TEXT_LENGTH"
   environment operation "numeric text exceeds the configured length limit")
  (let* ((length (length text))
         (position 0)
         (sign 1)
         (integer-part 0)
         (integer-digits 0)
         (fraction-part 0)
         (fraction-digits 0)
         (exponent 0)
         (exponent-sign 1)
         (exponent-digits 0)
         (exponent-too-large-p nil)
         (decimal-p nil)
         (exponent-p nil))
    (labels ((current-character ()
               (and (< position length) (char text position)))
             (consume-digits (consumer)
               (let ((count 0))
                 (loop for character = (current-character)
                       while (and character (digit-char-p character 10))
                       do (funcall consumer (digit-char-p character 10))
                          (incf position)
                          (incf count))
                 count))
             (invalid ()
               (%raise-syntax-number-error text environment operation)))
      (let ((character (current-character)))
        (when (and character (member character '(#\+ #\-) :test #'char=))
          (when (char= character #\-)
            (setf sign -1))
          (incf position)))
      (setf integer-digits
            (consume-digits (lambda (digit)
                              (setf integer-part (+ (* integer-part 10) digit)))))
      (when (and (current-character) (char= (current-character) #\.))
        (setf decimal-p t)
        (incf position)
        (setf fraction-digits
              (consume-digits (lambda (digit)
                                (setf fraction-part (+ (* fraction-part 10) digit))))))
      (when (and (current-character)
                 (member (current-character) '(#\e #\E) :test #'char=))
        (setf exponent-p t)
        (incf position)
        (let ((character (current-character)))
          (when (and character (member character '(#\+ #\-) :test #'char=))
            (when (char= character #\-)
              (setf exponent-sign -1))
            (incf position)))
        (setf exponent-digits
              (consume-digits
               (lambda (digit)
                 (unless exponent-too-large-p
                   (if (> exponent
                          (floor (- *max-prolog-numeric-lexeme-length* digit) 10))
                       (setf exponent-too-large-p t)
                       (setf exponent (+ (* exponent 10) digit))))))))
      (when (or (zerop integer-digits)
                (and decimal-p (zerop fraction-digits))
                (and exponent-p (zerop exponent-digits))
                (/= position length))
        (invalid))
      (when exponent-too-large-p
        (%raise-resource-error
         "EXPONENT_MAGNITUDE" environment operation
         "numeric exponent exceeds the configured magnitude limit"))
      (if (or decimal-p exponent-p)
          (let* ((signed-exponent (* exponent-sign exponent))
                 (positive-exponent (max 0 signed-exponent))
                 (negative-exponent (max 0 (- signed-exponent))))
            (when (or (> (+ integer-digits fraction-digits positive-exponent)
                         *max-prolog-numeric-lexeme-length*)
                      (> (+ fraction-digits negative-exponent)
                         *max-prolog-numeric-lexeme-length*))
              (%raise-resource-error
               "NUMBER_SIZE" environment operation
               "numeric value exceeds the configured exact-size limit"))
            (let* ((fraction-scale (expt 10 fraction-digits))
                   (significand (+ integer-part (/ fraction-part fraction-scale)))
                   (scaled (* sign significand
                              (expt 10 signed-exponent))))
              (coerce scaled 'double-float)))
          (* sign integer-part)))))
