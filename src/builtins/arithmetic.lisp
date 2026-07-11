(in-package #:cl-prolog)

(define-condition arithmetic-evaluation-error (prolog-evaluation-error)
  ((expression :initarg :expression :reader arithmetic-error-expression)
   (reason :initarg :reason :reader arithmetic-error-reason))
  (:report (lambda (condition stream)
             (format stream "Cannot evaluate Prolog arithmetic expression ~S: ~A."
                     (arithmetic-error-expression condition)
                     (arithmetic-error-reason condition))))
  (:documentation "Signalled when a Prolog arithmetic expression cannot be evaluated."))

(defun %arithmetic-error (expression reason &rest arguments)
  (let ((message (apply #'format nil reason arguments)))
    (error 'arithmetic-evaluation-error
           :expression expression
           :reason message
           :term (%iso-error-term (%iso-term "EVALUATION_ERROR" (%iso-atom "UNDEFINED"))
                                  (%iso-atom "ARITHMETIC") message)
           :environment nil)))

(defun %check-integer-operands (expression left right)
  (declare (cl:ignore expression))
  (unless (integerp left)
    (%raise-type-error "INTEGER" left nil (%iso-atom "ARITHMETIC")
                       "integer operand required"))
  (unless (integerp right)
    (%raise-type-error "INTEGER" right nil (%iso-atom "ARITHMETIC")
                       "integer operand required")))

(defun %check-nonzero-divisor (expression divisor)
  (when (zerop divisor)
    (%raise-evaluation-error "ZERO_DIVISOR" nil (%iso-atom "ARITHMETIC")
                             (format nil "division by zero in ~S" expression))))

(defun %arithmetic-operator-key (operator)
  (and (symbolp operator)
       (intern (symbol-name operator) :keyword)))

(defun %evaluate-binary-arithmetic (operator left right expression)
  (case (%arithmetic-operator-key operator)
    (:+ (+ left right))
    (:- (- left right))
    (:* (* left right))
    (:/ (%check-nonzero-divisor expression right) (/ left right))
    (:// (%check-integer-operands expression left right)
        (%check-nonzero-divisor expression right)
        (truncate left right))
    (:div (%check-integer-operands expression left right)
         (%check-nonzero-divisor expression right)
         (floor left right))
    (:rem (%check-integer-operands expression left right)
         (%check-nonzero-divisor expression right)
         (rem left right))
    (:mod (%check-integer-operands expression left right)
         (%check-nonzero-divisor expression right)
         (mod left right))
    (:min (min left right))
    (:max (max left right))
    ((:** :^) (expt left right))
    (otherwise (%arithmetic-error expression "unknown binary operator ~S" operator))))

(defun %evaluate-unary-arithmetic (operator argument expression)
  (case (%arithmetic-operator-key operator)
    (:+ argument)
    (:- (- argument))
    (:abs (abs argument))
    (:sign (signum argument))
    (:truncate (truncate argument))
    (:round (round argument))
    (:ceiling (ceiling argument))
    (:floor (floor argument))
    (:sqrt (sqrt argument))
    (otherwise (%arithmetic-error expression "unknown unary operator ~S" operator))))

(defun %evaluate-arithmetic-expression (expression environment)
  "Evaluate a ground arithmetic EXPRESSION after applying ENVIRONMENT."
  (let ((resolved (logic-substitute expression environment)))
    (labels ((evaluate (term)
               (cond
                 ((numberp term) term)
                  ((logic-var-p term)
                   (%raise-instantiation-error environment (%iso-atom "ARITHMETIC")
                                               "arithmetic expression is not ground"))
                  ((not (consp term))
                   (%raise-type-error "EVALUABLE" term environment
                                      (%iso-atom "ARITHMETIC")
                                      "number or arithmetic expression required"))
                  ((not (%proper-list-p term))
                   (%raise-type-error "EVALUABLE" term environment
                                      (%iso-atom "ARITHMETIC")
                                      "proper arithmetic expression required"))
                 (t
                  (let ((operator (first term))
                        (arguments (rest term)))
                    (case (length arguments)
                      (1 (%evaluate-unary-arithmetic
                          operator (evaluate (first arguments)) term))
                      (2 (%evaluate-binary-arithmetic
                          operator
                          (evaluate (first arguments))
                          (evaluate (second arguments))
                          term))
                      (otherwise
                       (%arithmetic-error term
                                          "operator ~S expects one or two arguments, got ~D"
                                          operator (length arguments)))))))))
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
