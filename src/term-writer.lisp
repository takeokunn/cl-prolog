;;;; Canonical, operator-aware Prolog term rendering.

(in-package #:cl-prolog)

(defconstant +compound-argument-priority+ 999)

(declaim (ftype function %write-prolog-term))

(defun %writer-operator-name (functor)
  (case functor
    (and '|,|)
    (or '|;|)
    (not '|\\+|)
    (otherwise functor)))

(defun %writer-operator-definition (functor arity)
  (let ((specifiers (ecase arity
                      (1 '(:fx :fy))
                      (2 '(:xfx :xfy :yfx)))))
    (find-if (lambda (definition)
               (member (operator-definition-specifier definition)
                       specifiers :test #'eq))
             (%operator-table-find *standard-operator-table*
                                   (%writer-operator-name functor)))))

(defun %write-prolog-variable (variable stream)
  (let ((name (symbol-name variable)))
    (write-string (if (> (length name) 1) (subseq name 1) "_") stream)))

(defun %plain-prolog-atom-name-p (name)
  (and (plusp (length name))
       (lower-case-p (char name 0))
       (every #'%identifier-character-p name)))

(defun %write-quoted-prolog-atom (name stream)
  (write-char #\' stream)
  (loop for character across name
        do (when (member character '(#\' #\\) :test #'char=)
             (write-char character stream))
           (write-char character stream))
  (write-char #\' stream))

(defun %write-prolog-atom (atom stream)
  (let ((name (string-downcase (symbol-name atom))))
    (cond
      ((eq atom '|!|) (write-char #\! stream))
      ((%plain-prolog-atom-name-p name) (write-string name stream))
      (t (%write-quoted-prolog-atom (symbol-name atom) stream)))))

(defun %write-prolog-number (number stream)
  (etypecase number
    (integer (princ number stream))
    (float
     (let ((representation (string-downcase (write-to-string number))))
       (loop for character across representation
             do (write-char (if (find character "dfsle" :test #'char=)
                                #\e
                                character)
                            stream))))))

(defun %write-prolog-list (term stream)
  (write-char #\[ stream)
  (loop with tail = term
        with firstp = t
        while (consp tail)
        do (unless firstp (write-char #\, stream))
           (%write-prolog-term (car tail) stream +compound-argument-priority+)
           (setf firstp nil
                 tail (cdr tail))
        finally
           (unless (null tail)
             (write-char #\| stream)
             (%write-prolog-term tail stream +compound-argument-priority+)))
  (write-char #\] stream))

(defun %write-prolog-prefix-operator (term definition stream context-priority)
  (let* ((priority (operator-definition-priority definition))
         (parenthesize (> priority context-priority))
         (argument-priority (if (eq :fx (operator-definition-specifier definition))
                                (1- priority)
                                priority)))
    (when parenthesize (write-char #\( stream))
    (write-string (%operator-lexeme definition) stream)
    (write-char #\Space stream)
    (%write-prolog-term (second term) stream argument-priority)
    (when parenthesize (write-char #\) stream))))

(defun %write-prolog-binary-operator (term definition stream context-priority)
  (let* ((priority (operator-definition-priority definition))
         (specifier (operator-definition-specifier definition))
         (parenthesize (> priority context-priority))
         (left-priority (if (eq specifier :yfx) priority (1- priority)))
         (right-priority (if (eq specifier :xfy) priority (1- priority))))
    (when parenthesize (write-char #\( stream))
    (%write-prolog-term (second term) stream left-priority)
    (format stream " ~A " (%operator-lexeme definition))
    (%write-prolog-term (third term) stream right-priority)
    (when parenthesize (write-char #\) stream))))

(defun %write-prolog-conditional (term stream context-priority softp)
  (let ((parenthesize (> 1100 context-priority)))
    (when parenthesize (write-char #\( stream))
    (%write-prolog-term (second term) stream 1049)
    (write-string (if softp " *-> " " -> ") stream)
    (%write-prolog-term (third term) stream 1050)
    (write-string " ; " stream)
    (%write-prolog-term (fourth term) stream 1100)
    (when parenthesize (write-char #\) stream))))

(defun %write-prolog-compound (term stream)
  (%write-prolog-atom (first term) stream)
  (write-char #\( stream)
  (loop for argument in (rest term)
        for firstp = t then nil
        do (unless firstp (write-char #\, stream))
           (%write-prolog-term argument stream +compound-argument-priority+))
  (write-char #\) stream))

(defun %write-prolog-term (term stream context-priority)
  (cond
    ((null term) (write-string "[]" stream))
    ((logic-var-p term) (%write-prolog-variable term stream))
    ((numberp term) (%write-prolog-number term stream))
    ((symbolp term) (%write-prolog-atom term stream))
    ((atom term) (error "Cannot write non-Prolog atomic value ~S." term))
    ((and (member (first term) '(if-then-else soft-if-then-else) :test #'eq)
          (= (length term) 4))
     (%write-prolog-conditional term stream context-priority
                                (eq (first term) 'soft-if-then-else)))
    ((and (symbolp (first term)) (= (length term) 2))
     (let ((definition (%writer-operator-definition (first term) 1)))
       (if definition
           (%write-prolog-prefix-operator term definition stream context-priority)
           (%write-prolog-compound term stream))))
    ((and (symbolp (first term)) (= (length term) 3))
     (let ((definition (%writer-operator-definition (first term) 2)))
       (if definition
           (%write-prolog-binary-operator term definition stream context-priority)
           (%write-prolog-compound term stream))))
    ((and (symbolp (first term)) (listp term)) (%write-prolog-compound term stream))
    (t (%write-prolog-list term stream))))

(defun write-prolog-term (term &optional (stream *standard-output*))
  "Write TERM to STREAM in canonical, parseable Prolog syntax and return TERM."
  (%write-prolog-term term stream +maximum-operator-priority+)
  term)

(defun prolog-term-string (term)
  "Return the canonical, parseable Prolog representation of TERM."
  (with-output-to-string (stream)
    (write-prolog-term term stream)))
