(in-package #:cl-prolog)

(defun %io-operation (name)
  (%iso-atom name))

(defun %io-resolve-term (term environment operation)
  (let ((value (logic-substitute term environment)))
    (when (logic-var-p value)
      (%raise-instantiation-error environment operation
                                  "I/O argument must be instantiated"))
    value))

(defun %io-option-name-p (term name)
  (and (symbolp term) (string-equal (symbol-name term) name)))

(defun %io-boolean (value environment operation)
  (cond
    ((%io-option-name-p value "true") t)
    ((%io-option-name-p value "false") nil)
    ((logic-var-p value)
     (%raise-instantiation-error environment operation
                                 "Option value must be instantiated"))
    (t (%raise-domain-error "STREAM_OPTION" value environment operation
                            "Expected true or false"))))

(defparameter +io-read-option-names+
  '("variables" "variable_names" "singletons" "syntax_errors"))

(defun %io-parse-option-list (term environment operation domain-name allowed
                              instantiation-predicate instantiation-message
                              list-message option-message unsupported-message)
  (let ((options (logic-substitute term environment))
        (result '()))
    (when (funcall instantiation-predicate options)
      (%raise-instantiation-error environment operation instantiation-message))
    (unless (%proper-list-p options)
      (%raise-type-error "LIST" options environment operation list-message))
    (dolist (option options (nreverse result))
      (unless (and (%proper-list-p option) (= (length option) 2)
                   (symbolp (first option)))
        (%raise-domain-error domain-name option environment operation option-message))
      (let ((name (find (symbol-name (first option)) allowed
                        :test #'string-equal)))
        (unless name
          (%raise-domain-error domain-name option environment operation
                               unsupported-message))
        (push (cons name (second option)) result)))))

(defun %io-options (term environment operation allowed)
  (%io-parse-option-list
   term environment operation "STREAM_OPTION" allowed
   #'%term-has-variables-p
   "Stream options must be instantiated"
   "Stream options must be a proper list"
   "Expected a unary stream option"
   "Unsupported stream option"))

(defun %io-option (name options &optional default)
  (let ((entry (assoc name options :test #'string-equal)))
    (if entry (cdr entry) default)))

(defun %io-read-options (term environment operation)
  (%io-parse-option-list
   term environment operation "READ_OPTION" +io-read-option-names+
   ;; READ options may carry output variables in option values
   ;; (e.g. variables/2 and variable_names/2), so only reject the
   ;; options list itself when it is still a logic variable.
   #'logic-var-p
   "Read options must be instantiated"
   "Read options must be a proper list"
   "Expected a unary read option"
   "Unsupported read option"))

(defun %io-syntax-errors-mode (options environment operation)
  (let ((mode (logic-substitute
               (%io-option "syntax_errors" options (%iso-atom "error"))
               environment)))
    (when (logic-var-p mode)
      (%raise-instantiation-error environment operation
                                  "syntax_errors must be instantiated"))
    (unless (and (symbolp mode)
                 (member (symbol-name mode) '("ERROR" "FAIL" "QUIET")
                         :test #'string-equal))
      (%raise-domain-error "READ_OPTION" mode environment operation
                           "syntax_errors must be error, fail, or quiet"))
    mode))

