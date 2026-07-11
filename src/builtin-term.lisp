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

(defun %compare-scalars (left right)
  (cond
    ((equal left right) 0)
    ((and (realp left) (realp right)) (if (< left right) -1 1))
    (t (if (string< (prin1-to-string left) (prin1-to-string right)) -1 1))))

(declaim (ftype (function (t t) integer) %compare-terms))

(defun %compare-term-sequences (left right)
  (loop for left-term in left
        for right-term in right
        for comparison = (%compare-terms left-term right-term)
        unless (zerop comparison) return comparison
        finally (return 0)))

(defun %compare-compound-terms (left right)
  (let ((arity-comparison (%compare-scalars (length (rest left))
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
          ((= left-class 3) (%compare-compound-terms left right))
          (t (%compare-scalars left right))))))

(defun %emit-term-comparison (predicate left right environment emit)
  (when (funcall predicate
                 (%compare-terms (%term-resolve left environment)
                                 (%term-resolve right environment)))
    (funcall emit environment)))

(define-builtin (var term) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (when (logic-var-p (%term-resolve term environment))
    (funcall emit environment)))

(define-builtin (nonvar term) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (unless (logic-var-p (%term-resolve term environment))
    (funcall emit environment)))

(define-builtin (atom term) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (when (%term-atom-p (%term-resolve term environment))
    (funcall emit environment)))

(define-builtin (number term) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (when (numberp (%term-resolve term environment))
    (funcall emit environment)))

(define-builtin (integer term) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (when (integerp (%term-resolve term environment))
    (funcall emit environment)))

(define-builtin (== left right) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (when (%term-identical-p (%term-resolve left environment)
                           (%term-resolve right environment))
    (funcall emit environment)))

(define-builtin (|\\==| left right) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (unless (%term-identical-p (%term-resolve left environment)
                             (%term-resolve right environment))
    (funcall emit environment)))

(define-builtin (@< left right) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (%emit-term-comparison #'minusp left right environment emit))

(define-builtin (@=< left right) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (%emit-term-comparison (lambda (comparison) (not (plusp comparison)))
                         left right environment emit))

(define-builtin (@> left right) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (%emit-term-comparison #'plusp left right environment emit))

(define-builtin (@>= left right) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (%emit-term-comparison (lambda (comparison) (not (minusp comparison)))
                         left right environment emit))

(define-builtin (compare order left right) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (%unify-emit order
               (ecase (%compare-terms (%term-resolve left environment)
                                      (%term-resolve right environment))
                 (-1 '<)
                 (0 '=)
                 (1 '>))
               environment emit))

(define-builtin (term-variables term variables) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (%unify-emit variables
               (%collect-variables (%term-resolve term environment))
               environment emit))

(define-builtin (compound term) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (when (%term-compound-p (%term-resolve term environment))
    (funcall emit environment)))

(define-builtin (ground term) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (unless (%term-has-variables-p (%term-resolve term environment))
    (funcall emit environment)))

(define-builtin (functor term name arity) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (let ((resolved-term (%term-resolve term environment)))
    (cond
      ((logic-var-p resolved-term)
       (let ((resolved-name (%term-resolve name environment))
             (resolved-arity (%term-resolve arity environment)))
         (when (and (or (%term-atom-p resolved-name) (numberp resolved-name))
                    (integerp resolved-arity)
                    (not (minusp resolved-arity))
                    (or (zerop resolved-arity) (%term-atom-p resolved-name)))
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
              environment emit)))))
      ((or (%term-atom-p resolved-term) (numberp resolved-term))
       (%term-unify-sequence
        (list (cons name resolved-term) (cons arity 0)) environment emit))
      ((%term-compound-p resolved-term)
       (%term-unify-sequence
        (list (cons name (first resolved-term))
              (cons arity (length (rest resolved-term))))
        environment emit)))))

(define-builtin (arg index term argument) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (let ((resolved-index (%term-resolve index environment))
        (resolved-term (%term-resolve term environment)))
    (when (and (integerp resolved-index)
               (plusp resolved-index)
               (%term-compound-p resolved-term)
               (<= resolved-index (length (rest resolved-term))))
      (%unify-emit argument (nth resolved-index resolved-term) environment emit))))

(define-builtin (copy-term source copy) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (%unify-emit copy
               (%freshen-term (%term-resolve source environment)
                              (make-hash-table :test #'eq))
               environment emit))

(define-builtin (|=..| term list) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (let ((resolved-term (%term-resolve term environment))
        (resolved-list (%term-resolve list environment)))
    (cond
      ((not (logic-var-p resolved-term))
       (let ((decomposition
               (cond
                 ((or (%term-atom-p resolved-term) (numberp resolved-term))
                  (list resolved-term))
                 ((%term-compound-p resolved-term) resolved-term))))
         (when decomposition
           (%unify-emit list decomposition environment emit))))
      ((and (%proper-list-p resolved-list) resolved-list)
       (let ((head (first resolved-list))
             (arguments (rest resolved-list)))
         (when (or (and (null arguments)
                        (or (%term-atom-p head) (numberp head)))
                   (and arguments (%term-atom-p head)))
           (%unify-emit term
                        (if arguments resolved-list head)
                        environment emit)))))))
