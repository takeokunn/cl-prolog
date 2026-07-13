;;;; Atom, character, and numeric text conversion builtins.
(in-package #:cl-prolog)

(defun %atom-text (atom)
  (symbol-name atom))

(defun %text-atom (text)
  (%prolog-atom-symbol text :preserve-case t))

(defun %character-atom-p (term)
  (and (%term-atom-p term) (= 1 (length (%atom-text term)))))

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
