(in-package #:cl-prolog)

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
