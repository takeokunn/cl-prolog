;;;; Unification and substitution.
;;;;
;;;; Environments are association lists mapping logic variables to terms.
;;;; They are persistent: UNIFY never mutates an environment, it extends it,
;;;; so backtracking is simply "keep using the older environment".

(in-package #:fx.prolog)

(defun logic-var-p (term)
  "Return true when TERM is a logic variable (a non-keyword ?-prefixed symbol)."
  (and (symbolp term)
       (not (keywordp term))
       (plusp (length (symbol-name term)))
       (char= (char (symbol-name term) 0) #\?)))

(defun fresh-logic-variable (&optional (prefix "?VAR"))
  "Return a fresh, never-before-seen logic variable."
  (gensym prefix))

(defun %walk-term (term env)
  "Chase TERM through ENV until it is unbound or not a variable."
  (loop while (logic-var-p term)
        for binding = (assoc term env :test #'eq)
        while binding
        do (setf term (cdr binding))
        finally (return term)))

(defun %occurs-p (var term env)
  "Return true when VAR occurs inside TERM under ENV (prevents cyclic terms)."
  (let ((term (%walk-term term env)))
    (cond
      ((eq var term) t)
      ((consp term)
       (or (%occurs-p var (car term) env)
           (%occurs-p var (cdr term) env)))
      (t nil))))

(defun unify (left right &optional (env '()))
  "Unify LEFT and RIGHT against ENV.

Returns (VALUES EXTENDED-ENV T) on success and (VALUES NIL NIL) on failure."
  (setf left (%walk-term left env)
        right (%walk-term right env))
  (cond
    ((eq left right) (values env t))
    ((logic-var-p left)
     (if (%occurs-p left right env)
         (values nil nil)
         (values (acons left right env) t)))
    ((logic-var-p right)
     (unify right left env))
    ((and (consp left) (consp right))
     (multiple-value-bind (extended ok)
         (unify (car left) (car right) env)
       (if ok
           (unify (cdr left) (cdr right) extended)
           (values nil nil))))
    ((equal left right) (values env t))
    (t (values nil nil))))

(defun logic-substitute (template env)
  "Recursively apply ENV to TEMPLATE, preserving dotted structure."
  (let ((resolved (%walk-term template env)))
    (if (consp resolved)
        (cons (logic-substitute (car resolved) env)
              (logic-substitute (cdr resolved) env))
        resolved)))

(defun %collect-variables (term)
  "Return the logic variables of TERM in first-appearance order."
  (let ((seen (make-hash-table :test #'eq))
        (variables '()))
    (labels ((walk (node)
               (cond
                 ((logic-var-p node)
                  (unless (gethash node seen)
                    (setf (gethash node seen) t)
                    (push node variables)))
                 ((consp node)
                  (walk (car node))
                  (walk (cdr node))))))
      (walk term))
    (nreverse variables)))

(defun %freshen-term (term table)
  "Copy TERM, replacing each logic variable via TABLE with a fresh one."
  (cond
    ((logic-var-p term)
     (or (gethash term table)
         (setf (gethash term table) (fresh-logic-variable "?FRESH"))))
    ((consp term)
     (cons (%freshen-term (car term) table)
           (%freshen-term (cdr term) table)))
    (t term)))

(defun %term-has-variables-p (term)
  "True when TERM contains at least one logic variable."
  (cond
    ((logic-var-p term) t)
    ((consp term)
     (or (%term-has-variables-p (car term))
         (%term-has-variables-p (cdr term))))
    (t nil)))

(defun %freshen-fact-args (fact)
  "Return FACT's argument list with any logic variables freshly renamed.

Ground facts (the common case) are returned as-is without consing."
  (let ((args (fact-args fact)))
    (if (%term-has-variables-p args)
        (%freshen-term args (make-hash-table :test #'eq))
        args)))

(defun %freshen-rule (rule)
  "Return RULE with all logic variables consistently renamed to fresh ones."
  (let ((table (make-hash-table :test #'eq)))
    (make-rule :head (%freshen-term (rule-head rule) table)
               :body (mapcar (lambda (goal)
                               (%freshen-term goal table))
                             (rule-body rule)))))
