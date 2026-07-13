;;;; Atom, character, and numeric text conversion builtins.
(in-package #:cl-prolog)

(define-list-conversion atom_chars atom %character-list-text %atom-character-list)

(define-list-conversion atom_codes atom %code-list-text %atom-code-list)

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
