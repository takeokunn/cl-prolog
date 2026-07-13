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

(defun %prolog-number-p (value)
  "Return true for numeric types representable by ISO Prolog arithmetic."
  (or (integerp value) (floatp value)))

(defun %require-prolog-number (value environment)
  (unless (%prolog-number-p value)
    (%raise-type-error "EVALUABLE" value environment (%iso-atom "ARITHMETIC")
                       "integer or float operand required")))

(defun %require-real (value)
  (unless (and (%prolog-number-p value) (realp value))
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
    (%require-real left) (%require-real right)
    (%check-nonzero-divisor expression right)
    (/ (float left 1.0d0) (float right 1.0d0)))
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

(defun %arithmetic-function-table (arity)
  (case arity
    (1 *unary-arithmetic-functions*)
    (2 *binary-arithmetic-functions*)))

(defun %require-arithmetic-function (operator arity expression environment)
  (let* ((table (%arithmetic-function-table arity))
         (entry (and table (assoc (%arithmetic-operator-key operator) table))))
    (unless entry
      (%raise-type-error
       "EVALUABLE" (%iso-term "/" operator arity) environment
       (%iso-atom "ARITHMETIC")
       (format nil "~S is not an evaluable arithmetic function" expression)))
    (values table entry)))

(defun %call-arithmetic-function (entry arguments expression)
  (handler-case
      (let ((result (apply (cdr entry) (append arguments (list expression)))))
        (unless (%prolog-number-p result)
          (%arithmetic-error expression
                             "arithmetic result is not an integer or float: ~S"
                             result))
        result)
    (cl:arithmetic-error (condition)
      (%arithmetic-error expression "host arithmetic failure: ~A" condition))))

(defun %evaluate-binary-arithmetic (entry left right expression)
  (%call-arithmetic-function entry (list left right) expression))

(defun %evaluate-unary-arithmetic (entry argument expression)
  (%call-arithmetic-function entry (list argument) expression))

(defun %evaluate-arithmetic-expression (expression environment)
  "Evaluate a ground arithmetic EXPRESSION after applying ENVIRONMENT."
  (let ((resolved (logic-substitute expression environment)))
    (labels ((evaluate (term)
             (cond
                 ((numberp term)
                  (%require-prolog-number term environment)
                  term)
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
                    (multiple-value-bind (table entry)
                        (%require-arithmetic-function
                         operator (length arguments) term environment)
                      (declare (cl:ignore table))
                      (case (length arguments)
                        (1 (%evaluate-unary-arithmetic
                            entry (evaluate (first arguments)) term))
                        (2 (%evaluate-binary-arithmetic
                            entry
                            (evaluate (first arguments))
                            (evaluate (second arguments))
                            term)))))))))
      (evaluate resolved))))

(progn
  (define-builtin (is result expression) (rulebase environment depth emit)
    (%unify-emit result
                 (%evaluate-arithmetic-expression expression environment)
                 environment emit))
  (defmacro define-arithmetic-comparison (name predicate)
    `(define-builtin (,name left right) (rulebase environment depth emit)
       (when (,predicate (%evaluate-arithmetic-expression left environment)
                         (%evaluate-arithmetic-expression right environment))
         (funcall emit environment)))))

(define-arithmetic-comparison |=:=| =)

(define-arithmetic-comparison |=\\=| /=)

(define-arithmetic-comparison < <)

(define-arithmetic-comparison =< <=)

(define-arithmetic-comparison > >)

(define-arithmetic-comparison >= >=)

(define-builtin (between low high value) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let* ((operation (%iso-atom "BETWEEN"))
         (resolved-low (logic-substitute low environment))
         (resolved-high (logic-substitute high environment))
         (resolved-value (logic-substitute value environment)))
    (when (or (logic-var-p resolved-low) (logic-var-p resolved-high))
      (%raise-instantiation-error environment operation
                                  "lower and upper bounds must be instantiated"))
    (dolist (argument (list resolved-low resolved-high resolved-value))
      (unless (or (logic-var-p argument) (integerp argument))
        (%raise-type-error "INTEGER" argument environment operation
                           "arguments must be integers")))
    (if (logic-var-p resolved-value)
        (loop for candidate from resolved-low to resolved-high
              do (%unify-emit value candidate environment emit))
        (when (<= resolved-low resolved-value resolved-high)
          (funcall emit environment)))))

(define-builtin (succ predecessor successor) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let* ((operation (%iso-atom "SUCC"))
         (resolved-predecessor (logic-substitute predecessor environment))
         (resolved-successor (logic-substitute successor environment)))
    (when (and (logic-var-p resolved-predecessor)
               (logic-var-p resolved-successor))
      (%raise-instantiation-error environment operation
                                  "one argument must be instantiated"))
    (dolist (argument (list resolved-predecessor resolved-successor))
      (unless (logic-var-p argument)
        (unless (integerp argument)
          (%raise-type-error "INTEGER" argument environment operation
                             "arguments must be integers"))
        (when (minusp argument)
          (%raise-domain-error "NOT_LESS_THAN_ZERO" argument environment operation
                               "arguments must not be negative"))))
    (cond
      ((not (logic-var-p resolved-predecessor))
       (%unify-emit successor (1+ resolved-predecessor) environment emit))
      ((plusp resolved-successor)
       (%unify-emit predecessor (1- resolved-successor) environment emit)))))
