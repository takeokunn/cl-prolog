;;;; Atom, character, and numeric text conversion builtins.

(in-package #:cl-prolog)

(defmacro define-number-list-conversion (name list-to-text text-to-list)
  `(define-builtin (,name number list) (rulebase environment depth emit)
     (declare (cl:ignore rulebase depth))
     (let* ((operation (%iso-atom ,(string-upcase (symbol-name name))))
            (resolved-number (logic-substitute number environment))
            (resolved-list (logic-substitute list environment)))
       (cond
         ((not (logic-var-p resolved-number))
          (unless (realp resolved-number)
            (%raise-type-error "NUMBER" resolved-number environment operation
                               "first argument must be a number"))
          (%unify-emit list (,text-to-list
                             (%text-atom
                              (%number-text resolved-number environment operation)))
                       environment emit))
         ((not (logic-var-p resolved-list))
          (%unify-emit number
                       (%text-number (,list-to-text resolved-list environment operation)
                                     environment operation)
                       environment emit))
         (t
          (%raise-instantiation-error environment operation
                                      "one argument must be instantiated"))))))

(define-number-list-conversion number_chars %character-list-text %atom-character-list)
(define-number-list-conversion number_codes %code-list-text %atom-code-list)

(define-builtin (atom_number atom number) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let* ((operation (%iso-atom "ATOM_NUMBER"))
         (resolved-atom (logic-substitute atom environment))
         (resolved-number (logic-substitute number environment)))
    (unless (logic-var-p resolved-atom)
      (%ensure-atom-value resolved-atom environment operation "first argument"))
    (unless (or (logic-var-p resolved-number) (realp resolved-number))
      (%raise-type-error "NUMBER" resolved-number environment operation
                         "second argument must be a number"))
    (cond
      ((not (logic-var-p resolved-atom))
       ;; Unlike number_chars/2 and number_codes/2, invalid atom text fails.
       (handler-case
           (%unify-emit number
                        (%text-number (%atom-text resolved-atom)
                                      environment operation)
                        environment emit)
         (prolog-domain-error () nil)))
      ((not (logic-var-p resolved-number))
       (%unify-emit atom
                    (%text-atom (%number-text resolved-number environment operation))
                    environment emit))
      (t
       (%raise-instantiation-error environment operation
                                   "one argument must be instantiated")))))
