(in-package #:fx.prolog)

(defun logic-var-p (term)
  (and (symbolp term)
       (not (keywordp term))
       (plusp (length (symbol-name term)))
       (char= (char (symbol-name term) 0) #\?)))

(defun variable-symbol-p (term)
  (logic-var-p term))

(defun %assoc-binding (var env)
  (and (listp env) (assoc var env :test #'eq)))

(defun %walk-term (term env)
  (loop
    while (logic-var-p term)
    for binding = (%assoc-binding term env)
    while binding
    do (setf term (cdr binding))
    finally (return term)))

(defun %occurs-check (var term env)
  (let ((term (%walk-term term env)))
    (cond
      ((eq var term) t)
      ((consp term)
       (or (%occurs-check var (car term) env)
           (%occurs-check var (cdr term) env)))
      (t nil))))

(defun %unify-pair (left right env)
  (setf left (%walk-term left env)
        right (%walk-term right env))
  (cond
    ((eq left right) (values env t))
    ((logic-var-p left)
     (if (%occurs-check left right env)
         (values nil nil)
         (values (acons left right env) t)))
    ((logic-var-p right)
     (if (%occurs-check right left env)
         (values nil nil)
         (values (acons right left env) t)))
    ((and (consp left) (consp right))
     (multiple-value-bind (env1 ok1)
         (%unify-pair (car left) (car right) env)
       (if ok1
           (%unify-pair (cdr left) (cdr right) env1)
           (values nil nil))))
    ((equal left right) (values env t))
    (t (values nil nil))))

(defconstant +unify-failure+ :unify-fail)

(defun unify (term1 term2 &optional (env '()))
  "Unify TERM1 and TERM2 against ENV. Returns ENV and success-p."
  (multiple-value-bind (next-env success-p)
      (%unify-pair term1 term2 env)
    (if success-p
        (values next-env t)
        (values +unify-failure+ nil))))

(defun unify-failed-p (result)
  (eq result +unify-failure+))

(defmacro when-unify-succeeds ((env-var left right &optional (env ''())) &body body)
  `(let ((,env-var (unify ,left ,right ,env)))
     (unless (unify-failed-p ,env-var)
       ,@body)))

(defmacro when-unify-fails ((left right &optional (env ''())) &body body)
  `(when (unify-failed-p (unify ,left ,right ,env))
     ,@body))

(defun occurs-check (var term env)
  (%occurs-check var term env))

(defun logic-substitute (template env)
  "Recursively apply ENV to TEMPLATE, preserving dotted structure."
  (let ((resolved (%walk-term template env)))
    (cond
      ((logic-var-p resolved) resolved)
      ((consp resolved)
       (cons (logic-substitute (car resolved) env)
             (logic-substitute (cdr resolved) env)))
      (t resolved))))

(defun substitute-term (template env)
  (logic-substitute template env))
