;;;; Atom, character, and numeric text conversion builtins.
(in-package #:cl-prolog)

(defmacro define-list-conversion (name value-kind list-to-text text-to-list)
  (let* ((value (ecase value-kind
                  (atom (quote atom))
                  (number (quote number))))
         (resolved-value (ecase value-kind
                           (atom (quote resolved-atom))
                           (number (quote resolved-number))))
         (validation (ecase value-kind
                       (atom `(%ensure-atom-value ,resolved-value environment operation "first argument"))
                       (number `(unless (realp ,resolved-value)
                                  (%raise-type-error "NUMBER" ,resolved-value environment operation
                                                     "first argument must be a number")))))
         (forward-value (ecase value-kind
                          (atom `(,text-to-list ,resolved-value))
                          (number `(,text-to-list
                                    (%text-atom
                                     (%number-text ,resolved-value environment operation))))))
         (reverse-value (ecase value-kind
                          (atom `(%text-atom
                                  (,list-to-text resolved-list environment operation)))
                          (number `(%text-number
                                    (,list-to-text resolved-list environment operation)
                                    environment operation)))))
    `(define-builtin (,name ,value list) (rulebase environment depth emit)
       (declare (cl:ignore rulebase depth))
       (let* ((operation (%iso-atom ,(string-upcase (symbol-name name))))
              (,resolved-value (logic-substitute ,value environment))
              (resolved-list (logic-substitute list environment)))
         (cond
           ((not (logic-var-p ,resolved-value))
            ,validation
            (%unify-emit list ,forward-value environment emit))
           ((not (logic-var-p resolved-list))
            (%unify-emit ,value ,reverse-value environment emit))
           (t
            (%raise-instantiation-error environment operation
                                        "one argument must be instantiated")))))))

