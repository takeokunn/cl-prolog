;;;; Unification and substitution.
;;;;
;;;; Environments are association lists mapping logic variables to terms.
;;;; They are persistent: UNIFY never mutates an environment, it extends it,
;;;; so backtracking is simply "keep using the older environment".

(in-package #:cl-prolog)

(defvar *logic-variable-ordinals* nil)
(defvar *next-logic-variable-ordinal* 0)

(defmacro %with-logic-variable-order (&body body)
  "Run BODY inside a variable-creation-order context.

An enclosing context is reused so nested queries (e.g. a builtin proving a
sub-query) keep the ordinals of variables created by their caller."
  `(if *logic-variable-ordinals*
       (progn ,@body)
       (let ((*logic-variable-ordinals* (make-hash-table :test #'eq))
             (*next-logic-variable-ordinal* 0))
         ,@body)))

(defun %register-logic-variable (variable)
  "Assign VARIABLE its stable creation ordinal and return VARIABLE."
  (unless *logic-variable-ordinals*
    (error "Logic variables require an active ordering context."))
  (multiple-value-bind (ordinal present-p)
      (gethash variable *logic-variable-ordinals*)
    (declare (ignore ordinal))
    (unless present-p
      (setf (gethash variable *logic-variable-ordinals*)
            (prog1 *next-logic-variable-ordinal*
              (incf *next-logic-variable-ordinal*)))))
  variable)

(defun %logic-variable-ordinal (variable)
  "Return VARIABLE's registered ordinal."
  (multiple-value-bind (ordinal present-p)
      (gethash variable *logic-variable-ordinals*)
    (unless present-p
      (error "Unregistered logic variable ~S." variable))
    ordinal))

(defun logic-var-p (term)
  "Return true when TERM is a logic variable (a non-keyword ?-prefixed symbol)."
  (and (symbolp term)
       (not (keywordp term))
       (plusp (length (symbol-name term)))
       (char= (char (symbol-name term) 0) #\?)))

(defun fresh-logic-variable (&optional (prefix "?VAR"))
  "Return a fresh, never-before-seen logic variable."
  (let ((variable (gensym prefix)))
    (if *logic-variable-ordinals*
        (%register-logic-variable variable)
        variable)))

(defun %walk-term (term env)
  "Chase TERM through ENV until it is unbound or not a variable."
  (loop while (logic-var-p term)
        for binding = (assoc term env :test #'eq)
        while binding
        do (setf term (cdr binding))
        finally (return term)))

(defun %occurs-p (var term env)
  "Return true when VAR occurs inside TERM under ENV (prevents cyclic terms)."
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((occurs-p (node)
               (let ((resolved (%walk-term node env)))
                 (cond
                   ((eq var resolved) t)
                   ((not (consp resolved)) nil)
                   ((gethash resolved seen) nil)
                   (t
                    (setf (gethash resolved seen) t)
                    (or (occurs-p (car resolved))
                        (occurs-p (cdr resolved))))))))
      (occurs-p term))))

(defun unify (left right &optional (env '()))
  "Unify LEFT and RIGHT against ENV.

Returns (VALUES EXTENDED-ENV T) on success and (VALUES NIL NIL) on failure."
  (let ((seen-pairs (make-hash-table :test #'eq)))
    (labels ((seen-pair-p (left right)
               (member right (gethash left seen-pairs) :test #'eq))
             (unify-terms (left right environment)
               (setf left (%walk-term left environment)
                     right (%walk-term right environment))
               (cond
                 ((eq left right) (values environment t))
                 ((logic-var-p left)
                  (if (%occurs-p left right environment)
                      (values nil nil)
                      (values (acons left right environment) t)))
                 ((logic-var-p right)
                  (unify-terms right left environment))
                 ((and (consp left) (consp right))
                  (if (seen-pair-p left right)
                      (values environment t)
                      (progn
                        (push right (gethash left seen-pairs))
                        (multiple-value-bind (extended ok)
                            (unify-terms (car left) (car right) environment)
                          (if ok
                              (unify-terms (cdr left) (cdr right) extended)
                              (values nil nil))))))
                 ((and (symbolp left) (symbolp right)
                       (string= (symbol-name left) (symbol-name right)))
                  (values environment t))
                 ((equal left right) (values environment t))
                 (t (values nil nil)))))
      (unify-terms left right env))))

(defun logic-substitute (template env)
  "Recursively apply ENV to TEMPLATE, preserving dotted structure."
  (let ((copies (make-hash-table :test #'eq)))
    (labels ((substitute-term (term)
               (let ((resolved (%walk-term term env)))
                 (if (consp resolved)
                     (or (gethash resolved copies)
                         (let ((copy (cons nil nil)))
                           (setf (gethash resolved copies) copy
                                 (car copy) (substitute-term (car resolved))
                                 (cdr copy) (substitute-term (cdr resolved)))
                           copy))
                     resolved))))
      (substitute-term template))))

(defun %collect-variables (term)
  "Return the logic variables of TERM in first-appearance order."
  (let ((seen (make-hash-table :test #'eq))
        (seen-conses (make-hash-table :test #'eq))
        (variables '()))
    (labels ((walk (node)
               (cond
                 ((logic-var-p node)
                  (when *logic-variable-ordinals*
                    (%register-logic-variable node))
                  (unless (gethash node seen)
                    (setf (gethash node seen) t)
                    (push node variables)))
                 ((consp node)
                  (unless (gethash node seen-conses)
                    (setf (gethash node seen-conses) t)
                    (walk (car node))
                    (walk (cdr node)))))))
      (walk term))
    (nreverse variables)))

(defun %freshen-term (term table)
  "Copy TERM, replacing each logic variable via TABLE with a fresh one."
  (let ((copies (make-hash-table :test #'eq)))
    (labels ((freshen (node)
               (cond
                 ((logic-var-p node)
                  (or (gethash node table)
                      (setf (gethash node table)
                            (fresh-logic-variable "?FRESH"))))
                 ((consp node)
                  (or (gethash node copies)
                      (let ((copy (cons nil nil)))
                        (setf (gethash node copies) copy
                              (car copy) (freshen (car node))
                              (cdr copy) (freshen (cdr node)))
                        copy)))
                 (t node))))
      (freshen term))))

(defun %term-has-variables-p (term)
  "True when TERM contains at least one logic variable."
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((has-variables-p (node)
               (cond
                 ((logic-var-p node) t)
                 ((not (consp node)) nil)
                 ((gethash node seen) nil)
                 (t
                  (setf (gethash node seen) t)
                  (or (has-variables-p (car node))
                      (has-variables-p (cdr node)))))))
      (has-variables-p term))))

(defun %freshen-clause (clause)
  "Return CLAUSE with all logic variables consistently renamed to fresh ones."
  (let ((table (make-hash-table :test #'eq)))
    (make-clause (%freshen-term (clause-head clause) table)
                 (mapcar (lambda (goal)
                           (%freshen-term goal table))
                         (clause-body clause)))))
