;;;; Term inspection and construction builtins.

(in-package #:cl-prolog)

(defmacro %define-term-unary-predicate (name predicate succeeds-when-match-p)
  `(define-builtin (,name term) (rulebase environment depth emit)
     (declare (cl:ignore rulebase depth))
     (let ((matches (,predicate (logic-substitute term environment))))
       (when (if ,succeeds-when-match-p matches (not matches))
         (funcall emit environment)))))

(declaim (ftype (function (t t &optional hash-table) integer) %compare-terms))

(%define-term-unary-predicate var logic-var-p t)

(%define-term-unary-predicate nonvar logic-var-p nil)

(%define-term-unary-predicate atom %term-atom-p t)

(define-builtin (atomic term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let ((resolved-term (logic-substitute term environment)))
    (when (or (%term-atom-p resolved-term) (%prolog-number-p resolved-term))
      (funcall emit environment))))

(%define-term-unary-predicate number %prolog-number-p t)

(%define-term-unary-predicate integer integerp t)

(%define-term-unary-predicate float floatp t)

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
