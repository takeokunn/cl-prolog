;;;; Term inspection and construction builtins.

(in-package #:cl-prolog)

(defun %term-resolve (term environment)
  (logic-substitute term environment))

(defun %term-atom-p (term)
  (and (symbolp term) (not (logic-var-p term))))

(defun %term-compound-p (term)
  (and (consp term)
       (%proper-list-p term)
       (%term-atom-p (first term))))

(defun %term-acyclic-p (term &optional (environment '()))
  "Return true when TERM contains no cycle through cons cells."
  (let ((active (make-hash-table :test #'eq))
        (complete (make-hash-table :test #'eq)))
    (labels ((walk (node)
               (let ((resolved (%walk-term node environment)))
                 (cond
                   ((atom resolved) t)
                   ((gethash resolved complete) t)
                   ((gethash resolved active) nil)
                   (t
                    (setf (gethash resolved active) t)
                    (let ((acyclic-p (and (walk (car resolved))
                                          (walk (cdr resolved)))))
                      (remhash resolved active)
                      (when acyclic-p
                        (setf (gethash resolved complete) t))
                      acyclic-p))))))
      (walk term))))

(defun %term-unify-sequence (pairs environment emit)
  (labels ((unify-next (remaining current)
             (if (null remaining)
                 (funcall emit current)
                 (multiple-value-bind (extended ok)
                     (unify (caar remaining) (cdar remaining) current)
                   (when ok
                     (unify-next (cdr remaining) extended))))))
    (unify-next pairs environment)))

(defun %term-identical-p (left right)
  (cond
    ((or (logic-var-p left) (logic-var-p right)) (eq left right))
    ((and (consp left) (consp right))
     (and (%term-identical-p (car left) (car right))
          (%term-identical-p (cdr left) (cdr right))))
    (t (eql left right))))

(defun %term-order-class (term)
  (cond
    ((logic-var-p term) 0)
    ((numberp term) 1)
    ((%term-atom-p term) 2)
    ((consp term) 3)
    (t 4)))

(defun %compare-strings (left right)
  (cond
    ((string< left right) -1)
    ((string> left right) 1)
    (t 0)))

(defun %compare-numbers (left right)
  (cond
    ((equal left right) 0)
    ((and (realp left) (realp right))
     (cond
       ((< left right) -1)
       ((> left right) 1)
       (t (%compare-strings (prin1-to-string left)
                            (prin1-to-string right)))))
    (t (%compare-strings (prin1-to-string left)
                         (prin1-to-string right)))))

(defun %compare-atoms (left right)
  (%compare-strings (symbol-name left) (symbol-name right)))

(defun %compare-variables (left right)
  (let ((left-ordinal (%logic-variable-ordinal left))
        (right-ordinal (%logic-variable-ordinal right)))
    (cond
      ((< left-ordinal right-ordinal) -1)
      ((> left-ordinal right-ordinal) 1)
      (t 0))))

(declaim (ftype (function (t t) integer) %compare-terms))

(defun %compare-term-sequences (left right)
  (loop for left-term in left
        for right-term in right
        for comparison = (%compare-terms left-term right-term)
        unless (zerop comparison) return comparison
        finally (return 0)))

(defun %compare-compound-terms (left right)
  (let ((arity-comparison (%compare-numbers (length (rest left))
                                            (length (rest right)))))
    (if (zerop arity-comparison)
        (let ((functor-comparison (%compare-terms (first left) (first right))))
          (if (zerop functor-comparison)
              (%compare-term-sequences (rest left) (rest right))
              functor-comparison))
        arity-comparison)))

(defun %compare-terms (left right)
  (if (%term-identical-p left right)
      0
      (let ((left-class (%term-order-class left))
            (right-class (%term-order-class right)))
        (cond
          ((< left-class right-class) -1)
          ((> left-class right-class) 1)
          ((= left-class 0) (%compare-variables left right))
          ((= left-class 1) (%compare-numbers left right))
          ((= left-class 2) (%compare-atoms left right))
          ((= left-class 3) (%compare-compound-terms left right))
          (t (error "Not a Prolog term: ~S" left))))))

(defun %emit-term-comparison (predicate left right environment emit)
  (when (funcall predicate
                 (%compare-terms (%term-resolve left environment)
                                 (%term-resolve right environment)))
    (funcall emit environment)))

(define-builtin (var term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (when (logic-var-p (%term-resolve term environment))
    (funcall emit environment)))

(define-builtin (nonvar term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (unless (logic-var-p (%term-resolve term environment))
    (funcall emit environment)))

(define-builtin (atom term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (when (%term-atom-p (%term-resolve term environment))
    (funcall emit environment)))

(define-builtin (atomic term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let ((resolved-term (%term-resolve term environment)))
    (when (or (%term-atom-p resolved-term) (numberp resolved-term))
      (funcall emit environment))))

(define-builtin (number term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (when (numberp (%term-resolve term environment))
    (funcall emit environment)))

(define-builtin (integer term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (when (integerp (%term-resolve term environment))
    (funcall emit environment)))

(define-builtin (float term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (when (floatp (%term-resolve term environment))
    (funcall emit environment)))

(define-builtin (== left right) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (when (%term-identical-p (%term-resolve left environment)
                           (%term-resolve right environment))
    (funcall emit environment)))

(define-builtin (|\\==| left right) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (unless (%term-identical-p (%term-resolve left environment)
                             (%term-resolve right environment))
    (funcall emit environment)))

(define-builtin (@< left right) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (%emit-term-comparison #'minusp left right environment emit))

(define-builtin (@=< left right) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (%emit-term-comparison (lambda (comparison) (not (plusp comparison)))
                         left right environment emit))

(define-builtin (@> left right) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (%emit-term-comparison #'plusp left right environment emit))

(define-builtin (@>= left right) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (%emit-term-comparison (lambda (comparison) (not (minusp comparison)))
                         left right environment emit))

(define-builtin (compare order left right) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (%unify-emit order
               (ecase (%compare-terms (%term-resolve left environment)
                                      (%term-resolve right environment))
                 (-1 '<)
                 (0 '=)
                 (1 '>))
               environment emit))

(defun %term-subsumes-p (general specific)
  "Return true when SPECIFIC is an instance of GENERAL without binding either."
  (let ((bindings (make-hash-table :test #'eq)))
    (labels ((matches-p (pattern value)
               (cond
                 ((logic-var-p pattern)
                  (multiple-value-bind (bound present-p)
                      (gethash pattern bindings)
                    (if present-p
                        (%term-identical-p bound value)
                        (progn
                          (setf (gethash pattern bindings) value)
                          t))))
                 ((or (logic-var-p value)
                      (atom pattern)
                      (atom value))
                  (%term-identical-p pattern value))
                 ((and (consp pattern) (consp value))
                  (and (matches-p (car pattern) (car value))
                       (matches-p (cdr pattern) (cdr value))))
                 (t (%term-identical-p pattern value)))))
      (matches-p general specific))))

(define-builtin (subsumes_term general specific)
    (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (when (%term-subsumes-p (%term-resolve general environment)
                          (%term-resolve specific environment))
    (funcall emit environment)))

(define-builtin (term_variables term variables) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (%unify-emit variables
               (%collect-variables (%term-resolve term environment))
               environment emit))

(define-builtin (compound term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (when (%term-compound-p (%term-resolve term environment))
    (funcall emit environment)))

(define-builtin (callable term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let ((resolved-term (%term-resolve term environment)))
    (when (or (%term-atom-p resolved-term)
              (%term-compound-p resolved-term))
      (funcall emit environment))))

(define-builtin (ground term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (unless (%term-has-variables-p (%term-resolve term environment))
    (funcall emit environment)))

(define-builtin (acyclic_term term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (when (%term-acyclic-p term environment)
    (funcall emit environment)))

(define-builtin (cyclic_term term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (unless (%term-acyclic-p term environment)
    (funcall emit environment)))

(define-builtin (functor term name arity) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let ((resolved-term (%term-resolve term environment))
        (operation (%iso-atom "FUNCTOR")))
    (cond
      ((logic-var-p resolved-term)
       (let ((resolved-name (%term-resolve name environment))
             (resolved-arity (%term-resolve arity environment)))
         (cond
           ((logic-var-p resolved-name)
            (%raise-instantiation-error
             environment operation "functor/3 requires an instantiated name"))
           ((logic-var-p resolved-arity)
            (%raise-instantiation-error
             environment operation "functor/3 requires an instantiated arity"))
           ((not (or (%term-atom-p resolved-name) (numberp resolved-name)))
            (%raise-type-error
             "ATOMIC" resolved-name environment operation
             "functor/3 name must be atomic"))
           ((not (integerp resolved-arity))
            (%raise-type-error
             "INTEGER" resolved-arity environment operation
             "functor/3 arity must be an integer"))
           ((minusp resolved-arity)
            (%raise-domain-error
             "NOT_LESS_THAN_ZERO" resolved-arity environment operation
             "functor/3 arity must not be negative"))
           ((and (plusp resolved-arity) (not (%term-atom-p resolved-name)))
            (%raise-type-error
             "ATOM" resolved-name environment operation
             "functor/3 compound name must be an atom"))
           (t
            (let ((constructed
                    (if (zerop resolved-arity)
                        resolved-name
                        (cons resolved-name
                              (loop repeat resolved-arity
                                    collect (fresh-logic-variable))))))
              (%term-unify-sequence
               (list (cons term constructed)
                     (cons name resolved-name)
                     (cons arity resolved-arity))
               environment emit))))))
      ((or (%term-atom-p resolved-term) (numberp resolved-term))
       (%term-unify-sequence
        (list (cons name resolved-term) (cons arity 0)) environment emit))
      ((%term-compound-p resolved-term)
       (%term-unify-sequence
        (list (cons name (first resolved-term))
              (cons arity (length (rest resolved-term))))
        environment emit))
      (t
       (%raise-type-error
        "CALLABLE" resolved-term environment operation
        "functor/3 term must be a Prolog term")))))

(define-builtin (arg index term argument) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let ((resolved-index (%term-resolve index environment))
        (resolved-term (%term-resolve term environment))
        (operation (%iso-atom "ARG")))
    (cond
      ((logic-var-p resolved-index)
       (%raise-instantiation-error
        environment operation "arg/3 requires an instantiated index"))
      ((not (integerp resolved-index))
       (%raise-type-error
        "INTEGER" resolved-index environment operation
        "arg/3 index must be an integer"))
      ((minusp resolved-index)
       (%raise-domain-error
        "NOT_LESS_THAN_ZERO" resolved-index environment operation
        "arg/3 index must not be negative"))
      ((logic-var-p resolved-term)
       (%raise-instantiation-error
        environment operation "arg/3 requires an instantiated term"))
      ((not (%term-compound-p resolved-term))
       (%raise-type-error
        "COMPOUND" resolved-term environment operation
        "arg/3 term must be compound"))
      ((and (plusp resolved-index)
            (<= resolved-index (length (rest resolved-term))))
       (%unify-emit argument (nth resolved-index resolved-term) environment emit)))))

(define-builtin (copy_term source copy) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (%unify-emit copy
               (%freshen-term (%term-resolve source environment)
                              (make-hash-table :test #'eq))
               environment emit))

(define-builtin (numbervars term start end) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let* ((resolved-term (%term-resolve term environment))
         (resolved-start (%term-resolve start environment))
         (operation (%iso-atom "NUMBERVARS")))
    (cond
      ((logic-var-p resolved-start)
       (%raise-instantiation-error
        environment operation "numbervars/3 requires an instantiated start"))
      ((not (integerp resolved-start))
       (%raise-type-error
        "INTEGER" resolved-start environment operation
        "numbervars/3 start must be an integer"))
      ((minusp resolved-start)
       (%raise-domain-error
        "NOT_LESS_THAN_ZERO" resolved-start environment operation
        "numbervars/3 start must not be negative"))
      (t
       (let ((variables (%collect-variables resolved-term)))
         (%term-unify-sequence
          (append
           (loop for variable in variables
                 for index from resolved-start
                 collect (cons variable (list '$var index)))
           (list (cons end (+ resolved-start (length variables)))))
          environment emit))))))

(define-builtin (|=..| term list) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let ((resolved-term (%term-resolve term environment))
        (resolved-list (%term-resolve list environment))
        (operation (%iso-atom "UNIV")))
    (cond
      ((not (logic-var-p resolved-term))
       (let ((decomposition
               (cond
                 ((or (%term-atom-p resolved-term) (numberp resolved-term))
                  (list resolved-term))
                 ((%term-compound-p resolved-term) resolved-term))))
         (if decomposition
             (%unify-emit list decomposition environment emit)
             (%raise-type-error
              "CALLABLE" resolved-term environment operation
              "=../2 term must be a Prolog term"))))
      ((logic-var-p resolved-list)
       (%raise-instantiation-error
        environment operation "=../2 requires at least one instantiated argument"))
      ((not (%proper-list-p resolved-list))
       (%raise-type-error
        "LIST" resolved-list environment operation
        "=../2 second argument must be a proper list"))
      ((null resolved-list)
       (%raise-domain-error
        "NON_EMPTY_LIST" resolved-list environment operation
        "=../2 second argument must not be empty"))
      (t
       (let ((head (first resolved-list))
             (arguments (rest resolved-list)))
         (cond
           ((logic-var-p head)
            (%raise-instantiation-error
             environment operation "=../2 requires an instantiated functor"))
           ((and arguments (not (%term-atom-p head)))
            (%raise-type-error
             "ATOM" head environment operation
             "=../2 compound functor must be an atom"))
           ((and (null arguments)
                 (not (or (%term-atom-p head) (numberp head))))
            (%raise-type-error
             "ATOMIC" head environment operation
             "=../2 singleton list must contain an atomic term"))
           (t
            (%unify-emit term
                         (if arguments resolved-list head)
                         environment emit))))))))
