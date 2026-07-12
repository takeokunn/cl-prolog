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

(defun %require-integer (value)
  (unless (integerp value)
    (%raise-type-error "INTEGER" value nil (%iso-atom "ARITHMETIC")
                       "integer operand required")))

(defun %require-real (value)
  (unless (realp value)
    (%raise-type-error "NUMBER" value nil (%iso-atom "ARITHMETIC")
                       "real operand required")))

(defun %check-nonzero-divisor (expression divisor)
  (when (zerop divisor)
    (%raise-evaluation-error "ZERO_DIVISOR" nil (%iso-atom "ARITHMETIC")
                             (format nil "division by zero in ~S" expression))))

(defun %arithmetic-operator-key (operator)
  (and (symbolp operator)
       (intern (symbol-name operator) :keyword)))

(defmacro define-arithmetic-table (name arity &body definitions)
  `(defparameter ,name
     (list ,@(loop for (operator parameters . body) in definitions
                   collect `(cons ,operator
                                  (lambda ,parameters
                                    (declare (cl:ignorable expression))
                                    ,@body))))
     ,(format nil "Data table for supported ~D-argument arithmetic functions." arity)))

(define-arithmetic-table *unary-arithmetic-functions* 1
  (:+ (value expression) value)
  (:- (value expression) (- value))
  (:abs (value expression) (abs value))
  (:sign (value expression) (signum value))
  (:signum (value expression) (signum value))
  (:truncate (value expression) (truncate value))
  (:round (value expression) (round value))
  (:ceiling (value expression) (ceiling value))
  (:floor (value expression) (floor value))
  (:float (value expression) (float value 1.0d0))
  (:float_integer_part (value expression)
    (%require-real value)
    (float (truncate value) value))
  (:float_fractional_part (value expression)
    (%require-real value)
    (- value (truncate value)))
  (:sqrt (value expression)
    (%require-real value)
    (when (minusp value)
      (%arithmetic-error expression "square root is undefined for ~S" value))
    (sqrt value))
  (:exp (value expression) (exp value))
  (:log (value expression)
    (%require-real value)
    (unless (plusp value)
      (%arithmetic-error expression "logarithm is undefined for ~S" value))
    ;; Call LOG through its function object: SBCL's compile-time interval
    ;; derivation for LOG evaluates float bounds, and on hosts with broken
    ;; FP-trap delivery that would otherwise make evaluation hang
    ;; COMPILE-FILE.  The dynamic call skips the derivation entirely.
    (funcall (symbol-function 'cl:log) value))
  (:sin (value expression) (sin value))
  (:cos (value expression) (cos value))
  (:tan (value expression) (tan value))
  (:asin (value expression) (asin value))
  (:acos (value expression) (acos value))
  (:atan (value expression) (atan value))
  (:|\\| (value expression)
    (%require-integer value)
    (lognot value)))

(define-arithmetic-table *binary-arithmetic-functions* 2
  (:+ (left right expression) (+ left right))
  (:- (left right expression) (- left right))
  (:* (left right expression) (* left right))
  (:/ (left right expression)
    (%check-nonzero-divisor expression right)
    (/ left right))
  (:// (left right expression)
    (%require-integer left) (%require-integer right)
    (%check-nonzero-divisor expression right)
    (truncate left right))
  (:div (left right expression)
    (%require-integer left) (%require-integer right)
    (%check-nonzero-divisor expression right)
    (floor left right))
  (:rem (left right expression)
    (%require-integer left) (%require-integer right)
    (%check-nonzero-divisor expression right)
    (rem left right))
  (:mod (left right expression)
    (%require-integer left) (%require-integer right)
    (%check-nonzero-divisor expression right)
    (mod left right))
  (:min (left right expression) (min left right))
  (:max (left right expression) (max left right))
  (:** (left right expression) (expt left right))
  (:^ (left right expression) (expt left right))
  (:|/\\| (left right expression)
    (%require-integer left) (%require-integer right)
    (logand left right))
  (:|\\/| (left right expression)
    (%require-integer left) (%require-integer right)
    (logior left right))
  (:xor (left right expression)
    (%require-integer left) (%require-integer right)
    (logxor left right))
  (:<< (left right expression)
    (%require-integer left) (%require-integer right)
    (ash left right))
  (:>> (left right expression)
    (%require-integer left) (%require-integer right)
    (ash left (- right)))
  (:atan (left right expression)
    (%require-real left) (%require-real right)
    (atan (float left 1.0d0) (float right 1.0d0))))

(defparameter *arithmetic-constants*
  (list (cons :pi pi))
  "Data table for zero-argument arithmetic constants.")

(defun %call-arithmetic-function (table operator arguments expression)
  (let ((entry (assoc (%arithmetic-operator-key operator) table)))
    (unless entry
      (%arithmetic-error expression "unknown arithmetic operator ~S" operator))
    (handler-case
        (apply (cdr entry) (append arguments (list expression)))
      (cl:arithmetic-error (condition)
        (%arithmetic-error expression "host arithmetic failure: ~A" condition)))))

(defun %evaluate-binary-arithmetic (operator left right expression)
  (%call-arithmetic-function *binary-arithmetic-functions*
                             operator (list left right) expression))

(defun %evaluate-unary-arithmetic (operator argument expression)
  (%call-arithmetic-function *unary-arithmetic-functions*
                             operator (list argument) expression))

(defun %evaluate-arithmetic-expression (expression environment)
  "Evaluate a ground arithmetic EXPRESSION after applying ENVIRONMENT."
  (let ((resolved (logic-substitute expression environment)))
    (labels ((evaluate (term)
               (cond
                 ((numberp term) term)
                 ((assoc (%arithmetic-operator-key term) *arithmetic-constants*)
                  (cdr (assoc (%arithmetic-operator-key term)
                              *arithmetic-constants*)))
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
