;;;; Builtin goal solvers.
;;;;
;;;; Every builtin is declared with DEFINE-BUILTIN and follows the engine's
;;;; CPS contract: emit each solution environment as it is found, never
;;;; collect intermediate result lists.

(in-package #:fx.prolog)

(defun %unify-emit (left right environment emit)
  "EMIT the extension of ENVIRONMENT that unifies LEFT with RIGHT, if any."
  (multiple-value-bind (extended ok)
      (unify left right environment)
    (when ok
      (funcall emit extended))))

(defun %proper-list-p (value)
  "True when VALUE is a fully-instantiated proper list."
  (cond
    ((null value) t)
    ((consp value) (%proper-list-p (cdr value)))
    (t nil)))

(define-condition arithmetic-evaluation-error (error)
  ((expression :initarg :expression :reader arithmetic-error-expression)
   (reason :initarg :reason :reader arithmetic-error-reason))
  (:report (lambda (condition stream)
             (format stream "Cannot evaluate Prolog arithmetic expression ~S: ~A."
                     (arithmetic-error-expression condition)
                     (arithmetic-error-reason condition))))
  (:documentation "Signalled when a Prolog arithmetic expression cannot be evaluated."))

(defun %arithmetic-error (expression reason &rest arguments)
  (error 'arithmetic-evaluation-error
         :expression expression
         :reason (apply #'format nil reason arguments)))

(defun %check-arithmetic-arity (expression arguments expected)
  (unless (= (length arguments) expected)
    (%arithmetic-error expression "operator ~S expects ~D argument~:P, got ~D"
                       (first expression) expected (length arguments))))

(defun %evaluate-arithmetic-expression (expression environment)
  "Evaluate a ground arithmetic EXPRESSION after applying ENVIRONMENT."
  (let ((resolved (logic-substitute expression environment)))
    (labels ((evaluate (term)
               (cond
                 ((numberp term) term)
                 ((logic-var-p term)
                  (%arithmetic-error expression "variable ~S is unbound" term))
                 ((not (consp term))
                  (%arithmetic-error expression "~S is not a number or arithmetic expression" term))
                 ((not (%proper-list-p term))
                  (%arithmetic-error expression "~S is not a proper arithmetic expression" term))
                 (t
                  (let ((operator (first term))
                        (arguments (rest term)))
                    (case operator
                      ((+ * / mod)
                       (%check-arithmetic-arity term arguments 2)
                       (let ((left (evaluate (first arguments)))
                             (right (evaluate (second arguments))))
                         (case operator
                           (+ (+ left right))
                           (* (* left right))
                           (/ (when (zerop right)
                                (%arithmetic-error expression "division by zero in ~S" term))
                              (/ left right))
                           (mod (unless (and (integerp left) (integerp right))
                                  (%arithmetic-error expression
                                                     "MOD operands must be integers in ~S" term))
                                (when (zerop right)
                                  (%arithmetic-error expression "division by zero in ~S" term))
                                (mod left right)))))
                      (-
                       (unless (member (length arguments) '(1 2))
                         (%arithmetic-error term "operator - expects one or two arguments, got ~D"
                                            (length arguments)))
                       (if (rest arguments)
                           (- (evaluate (first arguments))
                              (evaluate (second arguments)))
                           (- (evaluate (first arguments)))))
                      (abs
                       (%check-arithmetic-arity term arguments 1)
                       (abs (evaluate (first arguments))))
                      (otherwise
                       (%arithmetic-error expression "unknown arithmetic operator ~S" operator))))))))
      (evaluate resolved))))

;;; Control builtins

(define-builtin (!) (rulebase environment depth emit)
  (funcall emit environment)
  (%propagate-cut))

(define-builtin (= left right) (rulebase environment depth emit)
  (%unify-emit left right environment emit))

(define-builtin ((/= !=) left right) (rulebase environment depth emit)
  (unless (nth-value 1 (unify left right environment))
    (funcall emit environment)))

(define-builtin (not goal) (rulebase environment depth emit)
  (unless (%provable-p goal rulebase environment (1- depth))
    (funcall emit environment)))

(define-builtin (and &rest goals) (rulebase environment depth emit)
  (when (%prove-goal-sequence goals rulebase environment depth emit)
    (%propagate-cut)))

(define-builtin (or &rest alternatives) (rulebase environment depth emit)
  (dolist (alternative alternatives)
    (when (%prove-goal-sequence (%normalize-query alternative)
                                rulebase environment depth emit)
      (%propagate-cut))))

(define-builtin (:when test &rest variables) (rulebase environment depth emit)
  ;; TEST receives the solved value of each of VARIABLES.  The DSL compiles
  ;; (:when EXPR) guards into such functions; hand-written queries must pass
  ;; a function object too.
  (unless (functionp test)
    (%invalid-goal (list* :when test variables)
                   ":WHEN needs a guard function, got ~S (use the PROLOG ~
                    macro to compile expression guards)" test))
  (when (apply test
               (mapcar (lambda (variable)
                         (logic-substitute variable environment))
                       variables))
    (funcall emit environment)))

;;; Arithmetic builtins

(define-builtin (is result expression) (rulebase environment depth emit)
  (%unify-emit result
               (%evaluate-arithmetic-expression expression environment)
               environment emit))

(define-builtin (|=:=| left right) (rulebase environment depth emit)
  (when (= (%evaluate-arithmetic-expression left environment)
           (%evaluate-arithmetic-expression right environment))
    (funcall emit environment)))

(define-builtin (|=\=| left right) (rulebase environment depth emit)
  (unless (= (%evaluate-arithmetic-expression left environment)
             (%evaluate-arithmetic-expression right environment))
    (funcall emit environment)))

(define-builtin (< left right) (rulebase environment depth emit)
  (when (< (%evaluate-arithmetic-expression left environment)
           (%evaluate-arithmetic-expression right environment))
    (funcall emit environment)))

(define-builtin (=< left right) (rulebase environment depth emit)
  (when (<= (%evaluate-arithmetic-expression left environment)
            (%evaluate-arithmetic-expression right environment))
    (funcall emit environment)))

(define-builtin (> left right) (rulebase environment depth emit)
  (when (> (%evaluate-arithmetic-expression left environment)
           (%evaluate-arithmetic-expression right environment))
    (funcall emit environment)))

(define-builtin (>= left right) (rulebase environment depth emit)
  (when (>= (%evaluate-arithmetic-expression left environment)
            (%evaluate-arithmetic-expression right environment))
    (funcall emit environment)))

;;; List builtins

(define-builtin (member item list-term) (rulebase environment depth emit)
  (let ((elements (logic-substitute list-term environment)))
    (when (%proper-list-p elements)
      (dolist (element elements)
        (%unify-emit item element environment emit)))))

(define-builtin (append left right result) (rulebase environment depth emit)
  (let ((left-value (logic-substitute left environment))
        (result-value (logic-substitute result environment)))
    (cond
      ((%proper-list-p left-value)
       (%unify-emit result
                    (append left-value (logic-substitute right environment))
                    environment emit))
      ((%proper-list-p result-value)
       (loop for split from 0 to (length result-value)
             do (multiple-value-bind (extended ok)
                    (unify left (subseq result-value 0 split) environment)
                  (when ok
                    (%unify-emit right (nthcdr split result-value)
                                 extended emit))))))))

(define-builtin (reverse forward backward) (rulebase environment depth emit)
  (let ((forward-value (logic-substitute forward environment))
        (backward-value (logic-substitute backward environment)))
    (cond
      ((%proper-list-p forward-value)
       (%unify-emit backward (reverse forward-value) environment emit))
      ((%proper-list-p backward-value)
       (%unify-emit forward (reverse backward-value) environment emit)))))

(define-builtin (length list-term length-term) (rulebase environment depth emit)
  (let ((list-value (logic-substitute list-term environment))
        (length-value (logic-substitute length-term environment)))
    (cond
      ((%proper-list-p list-value)
       (%unify-emit length-term (length list-value) environment emit))
      ((typep length-value '(integer 0))
       (%unify-emit list-term
                    (loop repeat length-value collect (fresh-logic-variable))
                    environment emit)))))
