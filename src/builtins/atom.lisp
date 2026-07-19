;;;; Atom, character, and numeric text conversion builtins.

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
         (resolved-atom (%term-resolve atom environment))
         (resolved-length (%term-resolve length environment)))
    (%ensure-atom-value resolved-atom environment operation "atom_length/2 atom")
    (%ensure-nonnegative-integer-or-variable
     resolved-length environment operation "atom_length/2 length")
    (%check-atom-text-limit resolved-atom environment operation)
    (%unify-emit length (length (%atom-text resolved-atom)) environment emit)))

(define-builtin (atom_concat left right whole) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let* ((operation (%iso-atom "ATOM_CONCAT"))
         (resolved-left (%term-resolve left environment))
         (resolved-right (%term-resolve right environment))
         (resolved-whole (%term-resolve whole environment)))
    (dolist (value (list resolved-left resolved-right resolved-whole))
      (unless (or (logic-var-p value) (%term-atom-p value))
        (%raise-type-error "ATOM" value environment operation
                           "atom_concat/3 arguments must be atoms")))
    (dolist (value (list resolved-left resolved-right resolved-whole))
      (unless (logic-var-p value)
        (%check-atom-text-limit value environment operation)))
    (cond
      ((not (logic-var-p resolved-whole))
       (let* ((whole-text (%atom-text resolved-whole))
              (whole-size (length whole-text)))
         (cond
           ((and (not (logic-var-p resolved-left))
                 (not (logic-var-p resolved-right)))
            (let* ((left-text (%atom-text resolved-left))
                   (right-text (%atom-text resolved-right))
                   (left-size (length left-text))
                   (right-size (length right-text)))
              (when (and (= (+ left-size right-size) whole-size)
                         (string= left-text whole-text :end2 left-size)
                         (string= right-text whole-text :start2 left-size))
                (funcall emit environment))))
           ((not (logic-var-p resolved-left))
            (let* ((left-text (%atom-text resolved-left))
                   (left-size (length left-text)))
              (when (and (<= left-size whole-size)
                         (string= left-text whole-text :end2 left-size))
                (%unify-emit
                 right
                 (%text-atom (subseq whole-text left-size) environment operation)
                 environment emit))))
           ((not (logic-var-p resolved-right))
            (let* ((right-text (%atom-text resolved-right))
                   (right-size (length right-text))
                   (split (- whole-size right-size)))
              (when (and (not (minusp split))
                         (string= right-text whole-text :start2 split))
                (%unify-emit
                 left
                 (%text-atom (subseq whole-text 0 split) environment operation)
                 environment emit))))
           (t
            (%check-resource-limit
             (1+ whole-size) *max-prolog-derived-atom-candidates*
             "ATOM_CANDIDATES" environment operation
             "atom_concat/3 candidate count exceeds the configured limit")
            (loop for split from 0 to whole-size
                  do (%emit-pairs
                      (list
                       (cons left
                             (%text-atom
                              (subseq whole-text 0 split)
                              environment operation))
                       (cons right
                             (%text-atom
                              (subseq whole-text split)
                              environment operation)))
                      environment emit))))))
      ((and (not (logic-var-p resolved-left))
            (not (logic-var-p resolved-right)))
       (let* ((left-text (%atom-text resolved-left))
              (right-text (%atom-text resolved-right))
              (combined-size (+ (length left-text) (length right-text))))
         (%check-resource-limit
          combined-size *max-prolog-quoted-lexeme-length* "ATOM_LENGTH"
          environment operation "derived atom exceeds the configured length limit")
         (%unify-emit
          whole
          (%text-atom
           (concatenate 'string left-text right-text) environment operation)
          environment emit)))
      (t
       (%raise-instantiation-error environment operation
                                   "atom_concat/3 requires the whole atom or both parts")))))

(define-builtin (sub_atom atom before length after sub) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let* ((operation (%iso-atom "SUB_ATOM"))
         (resolved-atom (%term-resolve atom environment))
         (resolved-before (%term-resolve before environment))
         (resolved-length (%term-resolve length environment))
         (resolved-after (%term-resolve after environment))
         (resolved-sub (%term-resolve sub environment)))
    (%ensure-atom-value resolved-atom environment operation "sub_atom/5 atom")
    (%ensure-nonnegative-integer-or-variable
     resolved-before environment operation "before")
    (%ensure-nonnegative-integer-or-variable
     resolved-length environment operation "length")
    (%ensure-nonnegative-integer-or-variable
     resolved-after environment operation "after")
    (unless (or (logic-var-p resolved-sub) (%term-atom-p resolved-sub))
      (%raise-type-error "ATOM" resolved-sub environment operation
                         "sub_atom/5 subterm must be an atom"))
    (%check-atom-text-limit resolved-atom environment operation)
    (unless (logic-var-p resolved-sub)
      (%check-atom-text-limit resolved-sub environment operation))
    (let* ((text (%atom-text resolved-atom))
           (size (length text))
           (sub-bound-p (not (logic-var-p resolved-sub)))
           (sub-text (and sub-bound-p (%atom-text resolved-sub)))
           (sub-size (and sub-bound-p (length sub-text)))
           (fixed-length
             (cond
               ((not (logic-var-p resolved-length)) resolved-length)
               (sub-bound-p sub-size)
               ((and (not (logic-var-p resolved-before))
                     (not (logic-var-p resolved-after)))
                (- size resolved-before resolved-after))))
           (fixed-before
             (cond
               ((not (logic-var-p resolved-before)) resolved-before)
               ((and (not (logic-var-p resolved-after))
                     (integerp fixed-length))
                (- size resolved-after fixed-length)))))
      (labels ((bound-integer-matches-p (value candidate)
                 (or (logic-var-p value) (= value candidate)))
               (emit-candidate (candidate-before candidate-length)
                 (let ((candidate-after
                         (- size candidate-before candidate-length)))
                   (when
                       (and (not (minusp candidate-before))
                            (not (minusp candidate-length))
                            (not (minusp candidate-after))
                            (bound-integer-matches-p
                             resolved-before candidate-before)
                            (bound-integer-matches-p
                             resolved-length candidate-length)
                            (bound-integer-matches-p
                             resolved-after candidate-after)
                            (or (not sub-bound-p)
                                (and (= sub-size candidate-length)
                                     (string=
                                      sub-text text
                                      :start2 candidate-before
                                      :end2 (+ candidate-before
                                               candidate-length)))))
                     (%emit-pairs
                      (list
                       (cons before candidate-before)
                       (cons length candidate-length)
                       (cons after candidate-after)
                       (cons sub
                             (if sub-bound-p
                                 resolved-sub
                                 (%text-atom
                                  (subseq
                                   text candidate-before
                                   (+ candidate-before candidate-length))
                                  environment operation))))
                      environment emit)))))
        (cond
          ((and (integerp fixed-before) (integerp fixed-length))
           (emit-candidate fixed-before fixed-length))
          ((integerp fixed-before)
           (loop for candidate-length from 0 to (- size fixed-before)
                 do (emit-candidate fixed-before candidate-length)))
          ((integerp fixed-length)
           (loop for candidate-before from 0 to (- size fixed-length)
                 do (emit-candidate candidate-before fixed-length)))
          ((not (logic-var-p resolved-after))
           (loop for candidate-before from 0 to (- size resolved-after)
                 for candidate-length = (- size resolved-after candidate-before)
                 do (emit-candidate candidate-before candidate-length)))
          (t
           (%check-resource-limit
            (/ (* (1+ size) (+ size 2)) 2)
            *max-prolog-derived-atom-candidates*
            "ATOM_CANDIDATES" environment operation
            "sub_atom/5 candidate count exceeds the configured limit")
           (loop for candidate-before from 0 to size
                 do (loop
                      for candidate-length from 0 to (- size candidate-before)
                      do (emit-candidate
                          candidate-before candidate-length)))))))))

(defmacro define-atom-list-conversion (name list-to-text atom-to-list)
  `(define-builtin (,name atom list) (rulebase environment depth emit)
     (declare (cl:ignore rulebase depth))
     (let* ((operation (%iso-atom ,(string-upcase (symbol-name name))))
            (resolved-atom (%term-resolve atom environment))
            (resolved-list (%term-resolve list environment)))
       (cond
         ((not (logic-var-p resolved-atom))
          (%ensure-atom-value resolved-atom environment operation "first argument")
          (%unify-emit
           list (,atom-to-list resolved-atom environment operation)
           environment emit))
         ((not (logic-var-p resolved-list))
          (%unify-emit
           atom
           (%text-atom
            (,list-to-text resolved-list environment operation)
            environment operation)
           environment emit))
         (t
          (%raise-instantiation-error environment operation
                                      "one argument must be instantiated"))))))

(define-atom-list-conversion atom_chars %character-list-text %atom-character-list)
(define-atom-list-conversion atom_codes %code-list-text %atom-code-list)

(define-builtin (char_code character code) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let* ((operation (%iso-atom "CHAR_CODE"))
         (resolved-character (%term-resolve character environment))
         (resolved-code (%term-resolve code environment)))
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
         (%unify-emit
          character (%text-atom (string value) environment operation)
          environment emit)))
      (t
       (%raise-instantiation-error environment operation
                                   "char_code/2 requires one instantiated argument")))))

(defmacro define-number-list-conversion (name list-to-text text-to-list)
  `(define-builtin (,name number list) (rulebase environment depth emit)
     (declare (cl:ignore rulebase depth))
     (let* ((operation (%iso-atom ,(string-upcase (symbol-name name))))
            (resolved-number (%term-resolve number environment))
            (resolved-list (%term-resolve list environment)))
       (cond
         ((not (logic-var-p resolved-number))
          (unless (realp resolved-number)
            (%raise-type-error "NUMBER" resolved-number environment operation
                               "first argument must be a number"))
          (%unify-emit
           list
           (,text-to-list
            (%text-atom
             (%number-text resolved-number environment operation)
             environment operation)
            environment operation
            *max-prolog-numeric-lexeme-length* "NUMBER_TEXT_LENGTH")
           environment emit))
         ((not (logic-var-p resolved-list))
          (%unify-emit
           number
           (%text-number
            (,list-to-text
             resolved-list environment operation
             *max-prolog-numeric-lexeme-length* "NUMBER_TEXT_LENGTH")
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
         (resolved-atom (%term-resolve atom environment))
         (resolved-number (%term-resolve number environment)))
    (unless (logic-var-p resolved-atom)
      (%ensure-atom-value resolved-atom environment operation "first argument"))
    (unless (or (logic-var-p resolved-number) (realp resolved-number))
      (%raise-type-error "NUMBER" resolved-number environment operation
                         "second argument must be a number"))
    (unless (logic-var-p resolved-atom)
      (%check-atom-text-limit resolved-atom environment operation))
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
       (%unify-emit
        atom
        (%text-atom
         (%number-text resolved-number environment operation)
         environment operation)
        environment emit))
      (t
       (%raise-instantiation-error environment operation
                                   "one argument must be instantiated")))))
