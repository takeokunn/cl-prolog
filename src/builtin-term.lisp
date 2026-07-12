;;;; Term inspection and construction builtins.

(in-package #:cl-prolog)

(declaim (ftype (function (t t &optional hash-table) integer) %compare-terms))

(define-builtin (var term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (when (logic-var-p (logic-substitute term environment))
    (funcall emit environment)))

(define-builtin (nonvar term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (unless (logic-var-p (logic-substitute term environment))
    (funcall emit environment)))

(define-builtin (atom term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (when (%term-atom-p (logic-substitute term environment))
    (funcall emit environment)))

(define-builtin (atomic term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let ((resolved-term (logic-substitute term environment)))
    (when (or (%term-atom-p resolved-term) (%prolog-number-p resolved-term))
      (funcall emit environment))))

(define-builtin (number term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (when (%prolog-number-p (logic-substitute term environment))
    (funcall emit environment)))

(define-builtin (integer term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (when (integerp (logic-substitute term environment))
    (funcall emit environment)))

(define-builtin (float term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (when (floatp (logic-substitute term environment))
    (funcall emit environment)))

(progn
  (defmacro %define-term-relation (name comparison succeeds-when-match-p)
    `(define-builtin (,name left right) (rulebase environment depth emit)
       (declare (cl:ignore rulebase depth))
       (let ((matches (,comparison (logic-substitute left environment)
                                   (logic-substitute right environment))))
         (when (if ,succeeds-when-match-p matches (not matches))
           (funcall emit environment)))))
  (%define-term-relation == %term-identical-p t))

(%define-term-relation |\\==| %term-identical-p nil)

(%define-term-relation |=@=| %term-variant-p t)

(%define-term-relation |\\=@=| %term-variant-p nil)

(progn
  (defmacro %define-term-order-predicate (name predicate)
    `(define-builtin (,name left right) (rulebase environment depth emit)
       (declare (cl:ignore rulebase depth))
       (%emit-term-comparison ,predicate left right environment emit)))
  (%define-term-order-predicate @< #'minusp))

(%define-term-order-predicate @=< (lambda (comparison) (not (plusp comparison))))

(%define-term-order-predicate @> #'plusp)

(%define-term-order-predicate @>= (lambda (comparison) (not (minusp comparison))))

(define-builtin (compare order left right) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (%unify-emit order
               (ecase (%compare-terms (logic-substitute left environment)
                                      (logic-substitute right environment))
                 (-1 '<)
                 (0 '=)
                 (1 '>))
               environment emit))

(defun %unifier-equations (left right)
  "Return the trial unifier for LEFT and RIGHT without changing caller state."
  (multiple-value-bind (trial-environment unified-p)
      (unify left right)
    (when unified-p
      (values
       (loop for variable in (%collect-variables (list left right))
             for binding = (assoc variable trial-environment :test #'eq)
             when binding
               collect (list '= variable (cdr binding)))
       t))))

(define-builtin (unifiable left right unifier)
    (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let ((resolved-left (logic-substitute left environment))
        (resolved-right (logic-substitute right environment)))
    (multiple-value-bind (equations unifiable-p)
        (%unifier-equations resolved-left resolved-right)
      (when unifiable-p
        (%unify-emit unifier equations environment emit)))))

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
  (when (%term-subsumes-p (logic-substitute general environment)
                          (logic-substitute specific environment))
    (funcall emit environment)))

(define-builtin (term_variables term variables) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (%unify-emit variables
               (%collect-variables (logic-substitute term environment))
               environment emit))

(define-builtin (term_variables term variables tail)
    (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (%unify-emit variables
               (append (%collect-variables (logic-substitute term environment))
                       tail)
               environment emit))

(define-builtin (compound term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (when (%term-compound-p (logic-substitute term environment))
    (funcall emit environment)))

(define-builtin (callable term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let ((resolved-term (logic-substitute term environment)))
    (when (or (%term-atom-p resolved-term)
              (%term-compound-p resolved-term))
      (funcall emit environment))))

(define-builtin (ground term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (unless (%term-has-variables-p (logic-substitute term environment))
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
  (let ((resolved-term (logic-substitute term environment))
        (operation (%iso-atom "FUNCTOR")))
    (cond
      ((logic-var-p resolved-term)
       (let ((resolved-name (logic-substitute name environment))
             (resolved-arity (logic-substitute arity environment)))
         (cond
           ((logic-var-p resolved-name)
            (%raise-instantiation-error
             environment operation "functor/3 requires an instantiated name"))
           ((logic-var-p resolved-arity)
            (%raise-instantiation-error
             environment operation "functor/3 requires an instantiated arity"))
           ((not (or (%term-atom-p resolved-name)
                     (%prolog-number-p resolved-name)))
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
      ((or (%term-atom-p resolved-term) (%prolog-number-p resolved-term))
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
  (let ((resolved-index (logic-substitute index environment))
        (resolved-term (logic-substitute term environment))
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
               (%freshen-term (logic-substitute source environment)
                              (make-hash-table :test #'eq))
               environment emit))

(define-builtin (numbervars term start end) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let* ((resolved-term (logic-substitute term environment))
         (resolved-start (logic-substitute start environment))
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
  (let ((resolved-term (logic-substitute term environment))
        (resolved-list (logic-substitute list environment))
        (operation (%iso-atom "UNIV")))
    (cond
      ((not (logic-var-p resolved-term))
       (let ((decomposition
               (cond
                 ((or (%term-atom-p resolved-term)
                      (%prolog-number-p resolved-term))
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
                 (not (or (%term-atom-p head) (%prolog-number-p head))))
            (%raise-type-error
             "ATOMIC" head environment operation
             "=../2 singleton list must contain an atomic term"))
           (t
            (%unify-emit term
                         (if arguments resolved-list head)
                         environment emit))))))))
