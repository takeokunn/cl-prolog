(in-package #:cl-prolog)

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

(define-builtin (is result expression) (rulebase environment depth emit)
  (%unify-emit result
               (%evaluate-arithmetic-expression expression environment)
               environment emit))

(define-builtin (|=:=| left right) (rulebase environment depth emit)
  (when (= (%evaluate-arithmetic-expression left environment)
           (%evaluate-arithmetic-expression right environment))
    (funcall emit environment)))

(define-builtin (|=\\=| left right) (rulebase environment depth emit)
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
