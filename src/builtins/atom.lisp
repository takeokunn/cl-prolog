;;;; Atom, character, and numeric text conversion builtins.

(in-package #:cl-prolog)

(defun %atom-text (atom)
  (symbol-name atom))

(defun %text-atom (text)
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

(defun %ensure-proper-instantiated-list (value environment operation argument)
  (let ((visited (make-hash-table :test #'eq))
        (tail value))
    (loop
      (cond
        ((null tail) (return value))
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
         (setf tail (cdr tail)))))))

(defun %atom-character-list (atom)
  (map 'list (lambda (character) (%text-atom (string character)))
       (%atom-text atom)))

(defun %atom-code-list (atom)
  (map 'list #'char-code (%atom-text atom)))

(defun %character-list-text (characters environment operation)
  (%ensure-proper-instantiated-list characters environment operation "character list")
  (with-output-to-string (text)
    (dolist (character characters)
      (unless (%character-atom-p character)
        (%raise-type-error "CHARACTER" character environment operation
                           "atom_chars/2 and number_chars/2 require character atoms"))
      (write-char (char (%atom-text character) 0) text))))

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

(defun %code-list-text (codes environment operation)
  (%ensure-proper-instantiated-list codes environment operation "code list")
  (with-output-to-string (text)
    (dolist (code codes)
      (write-char (%code-character code environment operation) text))))

(defun %number-text (number environment operation)
  (cond
    ((integerp number)
     (write-to-string number :base 10 :radix nil :readably t))
    ((floatp number)
     (let ((text (write-to-string number :base 10 :radix nil :readably t)))
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
  (%raise-domain-error "NUMBER_TEXT" (%text-atom text) environment operation
                       "text is not a valid number"))

(defun %text-number (text environment operation)
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
              (consume-digits (lambda (digit)
                                (setf exponent (+ (* exponent 10) digit))))))
      (when (or (zerop integer-digits)
                (and decimal-p (zerop fraction-digits))
                (and exponent-p (zerop exponent-digits))
                (/= position length))
        (invalid))
      (if (or decimal-p exponent-p)
          (let* ((fraction-scale (expt 10 fraction-digits))
                 (significand (+ integer-part (/ fraction-part fraction-scale)))
                 (scaled (* sign significand
                            (expt 10 (* exponent-sign exponent)))))
            (coerce scaled 'double-float))
          (* sign integer-part)))))

(defun %emit-pairs (pairs environment emit)
  (labels ((continue-with (remaining current-environment)
             (if (endp remaining)
                 (funcall emit current-environment)
                 (multiple-value-bind (next-environment unified-p)
                     (unify (caar remaining) (cdar remaining)
                            current-environment)
                   (when unified-p
                     (continue-with (cdr remaining) next-environment))))))
    (continue-with pairs environment)))

(define-builtin (atom_length atom length) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let* ((operation (%iso-atom "ATOM_LENGTH"))
         (resolved-atom (logic-substitute atom environment))
         (resolved-length (logic-substitute length environment)))
    (%ensure-atom-value resolved-atom environment operation "atom_length/2 atom")
    (%ensure-nonnegative-integer-or-variable
     resolved-length environment operation "atom_length/2 length")
    (%unify-emit length (length (%atom-text resolved-atom)) environment emit)))

(define-builtin (atom_concat left right whole) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let* ((operation (%iso-atom "ATOM_CONCAT"))
         (resolved-left (logic-substitute left environment))
         (resolved-right (logic-substitute right environment))
         (resolved-whole (logic-substitute whole environment)))
    (dolist (value (list resolved-left resolved-right resolved-whole))
      (unless (or (logic-var-p value) (%term-atom-p value))
        (%raise-type-error "ATOM" value environment operation
                           "atom_concat/3 arguments must be atoms")))
    (cond
      ((not (logic-var-p resolved-whole))
       (let ((text (%atom-text resolved-whole)))
         (loop for split from 0 to (length text)
               do (%emit-pairs
                   (list (cons left (%text-atom (subseq text 0 split)))
                         (cons right (%text-atom (subseq text split))))
                   environment emit))))
      ((and (not (logic-var-p resolved-left))
            (not (logic-var-p resolved-right)))
       (%unify-emit whole
                    (%text-atom (concatenate 'string (%atom-text resolved-left)
                                             (%atom-text resolved-right)))
                    environment emit))
      (t
       (%raise-instantiation-error environment operation
                                   "atom_concat/3 requires the whole atom or both parts")))))

(define-builtin (sub_atom atom before length after sub) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let* ((operation (%iso-atom "SUB_ATOM"))
         (resolved-atom (logic-substitute atom environment))
         (resolved-before (logic-substitute before environment))
         (resolved-length (logic-substitute length environment))
         (resolved-after (logic-substitute after environment))
         (resolved-sub (logic-substitute sub environment)))
    (%ensure-atom-value resolved-atom environment operation "sub_atom/5 atom")
    (%ensure-nonnegative-integer-or-variable resolved-before environment operation "before")
    (%ensure-nonnegative-integer-or-variable resolved-length environment operation "length")
    (%ensure-nonnegative-integer-or-variable resolved-after environment operation "after")
    (unless (or (logic-var-p resolved-sub) (%term-atom-p resolved-sub))
      (%raise-type-error "ATOM" resolved-sub environment operation
                         "sub_atom/5 subterm must be an atom"))
    (let* ((text (%atom-text resolved-atom))
           (size (length text)))
      (loop for candidate-before from 0 to size
            do (loop for candidate-length from 0 to (- size candidate-before)
                     for candidate-after = (- size candidate-before candidate-length)
                     do (%emit-pairs
                         (list (cons before candidate-before)
                               (cons length candidate-length)
                               (cons after candidate-after)
                               (cons sub (%text-atom
                                          (subseq text candidate-before
                                                  (+ candidate-before candidate-length)))))
                         environment emit))))))

(defmacro define-atom-list-conversion (name list-to-text atom-to-list)
  `(define-builtin (,name atom list) (rulebase environment depth emit)
     (declare (cl:ignore rulebase depth))
     (let* ((operation (%iso-atom ,(string-upcase (symbol-name name))))
            (resolved-atom (logic-substitute atom environment))
            (resolved-list (logic-substitute list environment)))
       (cond
         ((not (logic-var-p resolved-atom))
          (%ensure-atom-value resolved-atom environment operation "first argument")
          (%unify-emit list (,atom-to-list resolved-atom) environment emit))
         ((not (logic-var-p resolved-list))
          (%unify-emit atom (%text-atom (,list-to-text resolved-list environment operation))
                       environment emit))
         (t
          (%raise-instantiation-error environment operation
                                      "one argument must be instantiated"))))))

(define-atom-list-conversion atom_chars %character-list-text %atom-character-list)
(define-atom-list-conversion atom_codes %code-list-text %atom-code-list)

(define-builtin (char_code character code) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let* ((operation (%iso-atom "CHAR_CODE"))
         (resolved-character (logic-substitute character environment))
         (resolved-code (logic-substitute code environment)))
    (cond
      ((not (logic-var-p resolved-character))
       (unless (%character-atom-p resolved-character)
         (%raise-type-error "CHARACTER" resolved-character environment operation
                            "char_code/2 requires a one-character atom"))
       (unless (or (logic-var-p resolved-code) (integerp resolved-code))
         (%raise-type-error "INTEGER" resolved-code environment operation
                            "char_code/2 code must be an integer"))
       (%unify-emit code (char-code (char (%atom-text resolved-character) 0))
                    environment emit))
      ((not (logic-var-p resolved-code))
       (let ((value (%code-character resolved-code environment operation)))
         (%unify-emit character (%text-atom (string value)) environment emit)))
      (t
       (%raise-instantiation-error environment operation
                                   "char_code/2 requires one instantiated argument")))))

(defmacro define-number-list-conversion (name list-to-text text-to-list)
  `(define-builtin (,name number list) (rulebase environment depth emit)
     (declare (cl:ignore rulebase depth))
     (let* ((operation (%iso-atom ,(string-upcase (symbol-name name))))
            (resolved-number (logic-substitute number environment))
            (resolved-list (logic-substitute list environment)))
       (cond
         ((not (logic-var-p resolved-number))
          (unless (realp resolved-number)
            (%raise-type-error "NUMBER" resolved-number environment operation
                               "first argument must be a number"))
          (%unify-emit list (,text-to-list
                             (%text-atom
                              (%number-text resolved-number environment operation)))
                       environment emit))
         ((not (logic-var-p resolved-list))
          (%unify-emit number
                       (%text-number (,list-to-text resolved-list environment operation)
                                     environment operation)
                       environment emit))
         (t
          (%raise-instantiation-error environment operation
                                      "one argument must be instantiated"))))))

(define-number-list-conversion number_chars %character-list-text %atom-character-list)
(define-number-list-conversion number_codes %code-list-text %atom-code-list)

(define-builtin (atom_number atom number) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let* ((operation (%iso-atom "ATOM_NUMBER"))
         (resolved-atom (logic-substitute atom environment))
         (resolved-number (logic-substitute number environment)))
    (unless (logic-var-p resolved-atom)
      (%ensure-atom-value resolved-atom environment operation "first argument"))
    (unless (or (logic-var-p resolved-number) (realp resolved-number))
      (%raise-type-error "NUMBER" resolved-number environment operation
                         "second argument must be a number"))
    (cond
      ((not (logic-var-p resolved-atom))
       ;; Unlike number_chars/2 and number_codes/2, invalid atom text fails.
       (handler-case
           (%unify-emit number
                        (%text-number (%atom-text resolved-atom)
                                      environment operation)
                        environment emit)
         (prolog-domain-error () nil)))
      ((not (logic-var-p resolved-number))
       (%unify-emit atom
                    (%text-atom (%number-text resolved-number environment operation))
                    environment emit))
      (t
       (%raise-instantiation-error environment operation
                                   "one argument must be instantiated")))))
