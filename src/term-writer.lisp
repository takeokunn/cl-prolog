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

(defun %write-quoted-prolog-atom (name stream)
  (write-char #\' stream)
  (loop for character across name
        do (when (member character '(#\' #\\) :test #'char=)
             (write-char character stream))
           (write-char character stream))
  (write-char #\' stream))

(defun %write-prolog-atom (atom stream quotedp)
  (let ((name (string-downcase (symbol-name atom))))
    (cond
      ((eq atom '|!|) (write-char #\! stream))
      ((%plain-prolog-atom-name-p name) (write-string name stream))
      (quotedp (%write-quoted-prolog-atom (symbol-name atom) stream))
      (t (write-string (symbol-name atom) stream)))))

(defun %numbered-variable-index (term)
  (when (and (consp term)
             (symbolp (first term))
             (string= "$VAR" (symbol-name (first term)))
             (consp (rest term))
             (null (cddr term))
             (typep (second term) '(integer 0)))
    (second term)))

(defun %write-numbered-variable (index stream)
  (multiple-value-bind (suffix letter) (floor index 26)
    (write-char (code-char (+ (char-code #\A) letter)) stream)
    (unless (zerop suffix)
      (princ suffix stream))))

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

(defun %write-prolog-list (term stream quotedp numbervarsp ignore-opsp)
  (write-char #\[ stream)
  (loop with tail = term
        with firstp = t
        while (consp tail)
        do (unless firstp (write-char #\, stream))
           (%write-prolog-term (car tail) stream +compound-argument-priority+
                               quotedp numbervarsp ignore-opsp)
           (setf firstp nil
                 tail (cdr tail))
        finally
           (unless (null tail)
             (write-char #\| stream)
             (%write-prolog-term tail stream +compound-argument-priority+
                                 quotedp numbervarsp ignore-opsp)))
  (write-char #\] stream))

(defun %write-prolog-prefix-operator
    (term definition stream context-priority quotedp numbervarsp ignore-opsp)
  (let* ((priority (operator-definition-priority definition))
         (parenthesize (> priority context-priority))
         (argument-priority (if (eq :fx (operator-definition-specifier definition))
                                (1- priority)
                                priority)))
    (when parenthesize (write-char #\( stream))
    (write-string (%operator-lexeme definition) stream)
    (write-char #\Space stream)
    (%write-prolog-term (second term) stream argument-priority
                        quotedp numbervarsp ignore-opsp)
    (when parenthesize (write-char #\) stream))))

(defun %write-prolog-binary-operator
    (term definition stream context-priority quotedp numbervarsp ignore-opsp)
  (let* ((priority (operator-definition-priority definition))
         (specifier (operator-definition-specifier definition))
         (parenthesize (> priority context-priority))
         (left-priority (if (eq specifier :yfx) priority (1- priority)))
         (right-priority (if (eq specifier :xfy) priority (1- priority))))
    (when parenthesize (write-char #\( stream))
    (%write-prolog-term (second term) stream left-priority
                        quotedp numbervarsp ignore-opsp)
    (format stream " ~A " (%operator-lexeme definition))
    (%write-prolog-term (third term) stream right-priority
                        quotedp numbervarsp ignore-opsp)
    (when parenthesize (write-char #\) stream))))

(defun %write-prolog-conditional
    (term stream context-priority softp quotedp numbervarsp ignore-opsp)
  "Render (SOFT-)IF-THEN-ELSE at the ISO priorities of the -> / *-> / ; xfy
operators it stands for: `;' at 1100, `->'/`*->' at 1050, and the condition
argument one below that so a bare `->'/`*->' on the left needs no parens."
  (let* ((semicolon-priority 1100)
         (arrow-priority 1050)
         (condition-priority (1- arrow-priority))
         (parenthesize (> semicolon-priority context-priority)))
    (when parenthesize (write-char #\( stream))
    (%write-prolog-term (second term) stream condition-priority
                        quotedp numbervarsp ignore-opsp)
    (write-string (if softp " *-> " " -> ") stream)
    (%write-prolog-term (third term) stream arrow-priority
                        quotedp numbervarsp ignore-opsp)
    (write-string " ; " stream)
    (%write-prolog-term (fourth term) stream semicolon-priority
                        quotedp numbervarsp ignore-opsp)
    (when parenthesize (write-char #\) stream))))

(defun %write-prolog-compound (term stream quotedp numbervarsp ignore-opsp)
  (%write-prolog-atom (first term) stream quotedp)
  (write-char #\( stream)
  (loop for argument in (rest term)
        for firstp = t then nil
        do (unless firstp (write-char #\, stream))
           (%write-prolog-term argument stream +compound-argument-priority+
                               quotedp numbervarsp ignore-opsp))
  (write-char #\) stream))

(defun %write-prolog-term
    (term stream context-priority quotedp numbervarsp ignore-opsp)
  (cond
    ((null term) (write-string "[]" stream))
    ((logic-var-p term) (%write-prolog-variable term stream))
    ((numberp term) (%write-prolog-number term stream))
    ((symbolp term) (%write-prolog-atom term stream quotedp))
    ((atom term) (error "Cannot write non-Prolog atomic value ~S." term))
    ((and numbervarsp (%numbered-variable-index term))
     (%write-numbered-variable (%numbered-variable-index term) stream))
    ((and (not ignore-opsp)
          (member (first term) '(if-then-else soft-if-then-else) :test #'eq)
          (= (length term) 4))
     (%write-prolog-conditional term stream context-priority
                                (eq (first term) 'soft-if-then-else)
                                quotedp numbervarsp ignore-opsp))
    ((and (not ignore-opsp) (symbolp (first term)) (= (length term) 2))
     (let ((definition (%writer-operator-definition (first term) 1)))
       (if definition
           (%write-prolog-prefix-operator term definition stream context-priority
                                          quotedp numbervarsp ignore-opsp)
           (%write-prolog-compound term stream quotedp numbervarsp ignore-opsp))))
    ((and (not ignore-opsp) (symbolp (first term)) (= (length term) 3))
     (let ((definition (%writer-operator-definition (first term) 2)))
       (if definition
           (%write-prolog-binary-operator term definition stream context-priority
                                          quotedp numbervarsp ignore-opsp)
           (%write-prolog-compound term stream quotedp numbervarsp ignore-opsp))))
    ((and (symbolp (first term)) (listp term))
     (%write-prolog-compound term stream quotedp numbervarsp ignore-opsp))
    (t (%write-prolog-list term stream quotedp numbervarsp ignore-opsp))))

(defun %write-prolog-term-with-options
    (term stream &key (quoted t) (numbervars nil) (ignore-ops nil))
  (%write-prolog-term term stream +maximum-operator-priority+
                      quoted numbervars ignore-ops))

(defun write-prolog-term (term &optional (stream *standard-output*))
  "Write TERM to STREAM in canonical, parseable Prolog syntax and return TERM."
  (%write-prolog-term-with-options term stream)
  term)

(defun prolog-term-string (term)
  "Return the canonical, parseable Prolog representation of TERM."
  (with-output-to-string (stream)
    (write-prolog-term term stream)))
