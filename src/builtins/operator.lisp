;;;; ISO operator declaration and enumeration predicates.

(in-package #:cl-prolog)

(defun %operator-priority (term environment operation &key allow-variable)
  (let ((value (logic-substitute term environment)))
    (cond
      ((logic-var-p value)
       (if allow-variable
           value
           (%raise-instantiation-error
            environment operation "operator priority must be instantiated")))
      ((not (integerp value))
       (%raise-type-error "INTEGER" value environment operation
                          "operator priority must be an integer"))
      ((not (<= 0 value +maximum-operator-priority+))
       (%raise-domain-error "OPERATOR_PRIORITY" value environment operation
                            "operator priority must be between 0 and 1200"))
      (t value))))

(defun %operator-specifier (term environment operation &key allow-variable)
  (let ((value (logic-substitute term environment)))
    (cond
      ((logic-var-p value)
       (if allow-variable
           value
           (%raise-instantiation-error
            environment operation "operator specifier must be instantiated")))
      ((not (symbolp value))
       (%raise-type-error "ATOM" value environment operation
                          "operator specifier must be an atom"))
      (t
       (let ((specifier (intern (symbol-name value) :keyword)))
         (unless (%valid-operator-specifier-p specifier)
           (%raise-domain-error "OPERATOR_SPECIFIER" value environment operation
                                "unknown operator specifier"))
         specifier)))))

(defun %operator-names (term environment operation)
  (let ((value (logic-substitute term environment)))
    (cond
      ((logic-var-p value)
       (%raise-instantiation-error
        environment operation "operator name must be instantiated"))
      ((symbolp value) (list value))
      ((not (%proper-list-p value))
       (%raise-type-error "LIST" value environment operation
                          "operator names must form a proper list"))
      (t
       (dolist (name value value)
         (cond
           ((logic-var-p name)
            (%raise-instantiation-error
             environment operation "every operator name must be instantiated"))
           ((not (symbolp name))
            (%raise-type-error "ATOM" name environment operation
                               "every operator name must be an atom"))))))))

(defun %operator-name-filter (term environment operation)
  (let ((value (logic-substitute term environment)))
    (cond
      ((logic-var-p value) value)
      ((symbolp value) value)
      (t (%raise-type-error "ATOM" value environment operation
                            "operator name must be an atom")))))

(defun %ensure-modifiable-operator-name (name environment operation)
  (when (string= (symbol-name name) ",")
    (%raise-permission-error
     "MODIFY" "OPERATOR" name environment operation
     "the comma operator cannot be modified"))
  name)

(defun %define-operators (table names priority specifier environment operation)
  (reduce (lambda (updated name)
            (%operator-table-define
             updated
             (%ensure-modifiable-operator-name name environment operation)
             priority specifier))
          names
          :initial-value table))

(defun %operator-definition-matches-p (definition priority specifier name)
  (and (or (logic-var-p priority)
           (= priority (operator-definition-priority definition)))
       (or (logic-var-p specifier)
           (eq specifier (operator-definition-specifier definition)))
       (or (logic-var-p name)
           (eq name (operator-definition-name definition)))))

(defun %emit-operator-definition (definition priority specifier name environment emit)
  (%unify-emit
   priority (operator-definition-priority definition) environment
   (lambda (priority-environment)
     (%unify-emit
      specifier (%iso-atom (symbol-name (operator-definition-specifier definition)))
      priority-environment
      (lambda (specifier-environment)
        (%unify-emit name (operator-definition-name definition)
                     specifier-environment emit))))))

(define-builtin (op priority specifier names) (rulebase environment depth emit)
  (declare (ignore depth))
  (let* ((operation (%iso-atom "OP"))
         (resolved-priority (%operator-priority priority environment operation))
         (resolved-specifier (%operator-specifier specifier environment operation))
         (resolved-names (%operator-names names environment operation))
         (updated (%define-operators (rulebase-operator-table rulebase)
                                     resolved-names resolved-priority
                                     resolved-specifier environment operation)))
    (setf (rulebase-operator-table rulebase) updated)
    (funcall emit environment)))

(define-builtin (current_op priority specifier name) (rulebase environment depth emit)
  (declare (ignore depth))
  (let* ((operation (%iso-atom "CURRENT_OP"))
         (resolved-priority
           (%operator-priority priority environment operation :allow-variable t))
         (resolved-specifier
           (%operator-specifier specifier environment operation :allow-variable t))
         (resolved-name (%operator-name-filter name environment operation)))
    (dolist (definition (%operator-table-current (rulebase-operator-table rulebase)))
      (when (%operator-definition-matches-p
             definition resolved-priority resolved-specifier resolved-name)
        (%emit-operator-definition
         definition priority specifier name environment emit)))))
