;;;; Term ordering, identity, and variance: the standard order of terms
;;;; (==, \==, =@=, \=@=, @<, @=<, @>, @>=, compare, unifiable,
;;;; subsumes_term) and the comparison primitives they share.

(in-package #:cl-prolog)

(defun %term-identical-p (left right &optional
                                       (seen (make-hash-table :test #'eq)))
  (cond
    ((eq left right) t)
    ((or (logic-var-p left) (logic-var-p right)) nil)
    ((and (consp left) (consp right))
     (let ((right-terms (or (gethash left seen)
                            (setf (gethash left seen)
                                  (make-hash-table :test #'eq)))))
       (or (gethash right right-terms)
           (progn
             (setf (gethash right right-terms) t)
             (and (%term-identical-p (car left) (car right) seen)
                  (%term-identical-p (cdr left) (cdr right) seen))))))
    (t (eql left right))))

(defun %term-variant-p (left right)
  (let ((left-bindings (make-hash-table :test #'eq))
        (right-bindings (make-hash-table :test #'eq))
        (seen (make-hash-table :test #'eq)))
    (labels ((variants-p (left-term right-term)
               (cond
                 ((logic-var-p left-term)
                  (and (logic-var-p right-term)
                       (multiple-value-bind (right-binding left-bound-p)
                           (gethash left-term left-bindings)
                         (multiple-value-bind (left-binding right-bound-p)
                             (gethash right-term right-bindings)
                           (cond
                             ((or left-bound-p right-bound-p)
                              (and left-bound-p right-bound-p
                                   (eq right-binding right-term)
                                   (eq left-binding left-term)))
                             (t
                              (setf (gethash left-term left-bindings) right-term
                                    (gethash right-term right-bindings) left-term)
                              t))))))
                 ((logic-var-p right-term) nil)
                 ((and (consp left-term) (consp right-term))
                  (let ((right-terms
                          (or (gethash left-term seen)
                              (setf (gethash left-term seen)
                                    (make-hash-table :test #'eq)))))
                    (or (gethash right-term right-terms)
                        (progn
                          (setf (gethash right-term right-terms) t)
                          (and (variants-p (car left-term) (car right-term))
                               (variants-p (cdr left-term) (cdr right-term)))))))
                 ((or (consp left-term) (consp right-term)) nil)
                 (t (eql left-term right-term)))))
      (variants-p left right))))

(defun %term-order-class (term)
  (cond
    ((logic-var-p term) 0)
    ((%prolog-number-p term) 1)
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

(defvar *atom-order-ordinals* (make-hash-table :test #'eq))

(defvar *next-atom-order-ordinal* 0)

(defun %atom-order-ordinal (atom)
  (multiple-value-bind (ordinal presentp)
      (gethash atom *atom-order-ordinals*)
    (if presentp
        ordinal
        (setf (gethash atom *atom-order-ordinals*)
              (prog1 *next-atom-order-ordinal*
                (incf *next-atom-order-ordinal*))))))

(defun %compare-atoms (left right)
  (let ((name-comparison (%compare-strings (symbol-name left)
                                           (symbol-name right))))
    (if (zerop name-comparison)
        (let ((package-comparison
                (%compare-strings (if (symbol-package left)
                                      (package-name (symbol-package left))
                                      "")
                                  (if (symbol-package right)
                                      (package-name (symbol-package right))
                                      ""))))
          (if (zerop package-comparison)
              (%compare-numbers (%atom-order-ordinal left)
                                (%atom-order-ordinal right))
              package-comparison))
        name-comparison)))

(defun %compare-variables (left right)
  (let ((left-ordinal (%logic-variable-ordinal left))
        (right-ordinal (%logic-variable-ordinal right)))
    (cond
      ((< left-ordinal right-ordinal) -1)
      ((> left-ordinal right-ordinal) 1)
      (t 0))))

(declaim (ftype (function (t t &optional hash-table) integer) %compare-terms))

(defun %compare-term-sequences (left right seen)
  (loop for left-term in left
        for right-term in right
        for comparison = (%compare-terms left-term right-term seen)
        unless (zerop comparison) return comparison
        finally (return 0)))

(defun %compare-compound-terms (left right seen)
  (let ((arity-comparison (%compare-numbers (length (rest left))
                                            (length (rest right)))))
    (if (zerop arity-comparison)
        (let ((functor-comparison (%compare-terms (first left) (first right)
                                                  seen)))
          (if (zerop functor-comparison)
              (%compare-term-sequences (rest left) (rest right) seen)
              functor-comparison))
        arity-comparison)))

(defun %compare-cons-terms (left right seen)
  (let ((right-terms (or (gethash left seen)
                         (setf (gethash left seen)
                               (make-hash-table :test #'eq)))))
    (if (gethash right right-terms)
        0
        (progn
          (setf (gethash right right-terms) t)
          (let ((car-comparison (%compare-terms (car left) (car right) seen)))
            (if (zerop car-comparison)
                (%compare-terms (cdr left) (cdr right) seen)
                car-comparison))))))

(defun %compare-terms (left right &optional
                                    (seen (make-hash-table :test #'eq)))
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
          ((= left-class 3)
           (if (and (%proper-list-p left) (%proper-list-p right))
               (%compare-compound-terms left right seen)
               (%compare-cons-terms left right seen)))
          (t (error "Not a Prolog term: ~S" left))))))

(defun %emit-term-comparison (predicate left right environment emit)
  (when (funcall predicate
                 (%compare-terms (%term-resolve left environment)
                                 (%term-resolve right environment)))
    (funcall emit environment)))

(defmacro define-term-relation-builtin (&body definitions)
  "Define ISO builtins that test a binary relation between two resolved
terms.  Each of DEFINITIONS is (NAME PREDICATE &key NEGATE); the builtin
succeeds, without bindings, when (funcall PREDICATE resolved-left
resolved-right) holds, or fails to hold when NEGATE is true."
  `(progn
     ,@(loop for (name predicate &key negate) in definitions
             collect `(define-builtin (,name left right) (rulebase environment depth emit)
                        (declare (cl:ignore rulebase depth))
                        (,(if negate 'unless 'when)
                         (,predicate (%term-resolve left environment)
                                     (%term-resolve right environment))
                         (funcall emit environment))))))

(define-term-relation-builtin
  (== %term-identical-p)
  (|\\==| %term-identical-p :negate t)
  (|=@=| %term-variant-p)
  (|\\=@=| %term-variant-p :negate t))

(defmacro define-term-order-builtin (&body definitions)
  "Define ISO builtins testing the standard order of terms.  Each of
DEFINITIONS is (NAME PREDICATE); the builtin succeeds when PREDICATE holds
for the two resolved terms' %COMPARE-TERMS result."
  `(progn
     ,@(loop for (name predicate) in definitions
             collect `(define-builtin (,name left right)
                          (rulebase environment depth emit)
                        (declare (cl:ignore rulebase depth))
                        (%emit-term-comparison ,predicate left right
                                               environment emit)))))

(define-term-order-builtin
  (@< #'minusp)
  (@=< (lambda (comparison) (not (plusp comparison))))
  (@> #'plusp)
  (@>= (lambda (comparison) (not (minusp comparison)))))

(define-builtin (compare order left right) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (%unify-emit order
               (ecase (%compare-terms (%term-resolve left environment)
                                      (%term-resolve right environment))
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
  (let ((resolved-left (%term-resolve left environment))
        (resolved-right (%term-resolve right environment)))
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

(define-term-relation-builtin
  (subsumes_term %term-subsumes-p))
