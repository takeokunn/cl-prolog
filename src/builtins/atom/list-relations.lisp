;;;; Atom, character, and numeric text conversion builtins.

(in-package #:cl-prolog)

(defmacro define-atom-list-conversion (name list-to-text atom-to-list)
  `(define-builtin (,name atom list) (rulebase environment depth emit)
     (declare (cl:ignore rulebase depth))
     (let* ((operation (%iso-atom ,(string-upcase (symbol-name name))))
            (resolved-atom (logic-substitute atom environment))
            (resolved-list (logic-substitute list environment)))
       (cond
         ((not (logic-var-p resolved-atom))
          (%ensure-atom-value resolved-atom environment operation "first argument")
          (%unify-emit list (,atom-to-list resolved-atom) environment emit))
         ((not (logic-var-p resolved-list))
          (%unify-emit atom (%text-atom (,list-to-text resolved-list environment operation))
                       environment emit))
         (t
          (%raise-instantiation-error environment operation
                                      "one argument must be instantiated"))))))

(define-atom-list-conversion atom_chars %character-list-text %atom-character-list)
(define-atom-list-conversion atom_codes %code-list-text %atom-code-list)

(define-builtin (char_code character code) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let* ((operation (%iso-atom "CHAR_CODE"))
         (resolved-character (logic-substitute character environment))
         (resolved-code (logic-substitute code environment)))
    (cond
      ((not (logic-var-p resolved-character))
       (unless (%character-atom-p resolved-character)
         (%raise-type-error "CHARACTER" resolved-character environment operation
                            "char_code/2 requires a one-character atom"))
       (unless (or (logic-var-p resolved-code) (integerp resolved-code))
         (%raise-type-error "INTEGER" resolved-code environment operation
                            "char_code/2 code must be an integer"))
       (%unify-emit code (char-code (char (%atom-text resolved-character) 0))
                    environment emit))
      ((not (logic-var-p resolved-code))
       (let ((value (%code-character resolved-code environment operation)))
         (%unify-emit character (%text-atom (string value)) environment emit)))
      (t
       (%raise-instantiation-error environment operation
                                   "char_code/2 requires one instantiated argument")))))

