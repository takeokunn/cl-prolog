;;;; Atom, character, and numeric text conversion builtins.
(in-package #:cl-prolog)

(defun %emit-pairs (pairs environment emit)
  (labels ((continue-with (remaining current-environment)
             (if (endp remaining)
                 (funcall emit current-environment)
                 (multiple-value-bind (next-environment unified-p)
                     (unify (caar remaining) (cdar remaining)
                            current-environment)
                   (when unified-p
                     (continue-with (cdr remaining) next-environment))))))
    (continue-with pairs environment)))

(define-builtin (atom_length atom length) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let* ((operation (%iso-atom "ATOM_LENGTH"))
         (resolved-atom (logic-substitute atom environment))
         (resolved-length (logic-substitute length environment)))
    (%ensure-atom-value resolved-atom environment operation "atom_length/2 atom")
    (%ensure-nonnegative-integer-or-variable
     resolved-length environment operation "atom_length/2 length")
    (%unify-emit length (length (%atom-text resolved-atom)) environment emit)))

(define-builtin (atom_concat left right whole) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let* ((operation (%iso-atom "ATOM_CONCAT"))
         (resolved-left (logic-substitute left environment))
         (resolved-right (logic-substitute right environment))
         (resolved-whole (logic-substitute whole environment)))
    (dolist (value (list resolved-left resolved-right resolved-whole))
      (unless (or (logic-var-p value) (%term-atom-p value))
        (%raise-type-error "ATOM" value environment operation
                           "atom_concat/3 arguments must be atoms")))
    (cond
      ((not (logic-var-p resolved-whole))
       (let ((text (%atom-text resolved-whole)))
         (loop for split from 0 to (length text)
               do (%emit-pairs
                   (list (cons left (%text-atom (subseq text 0 split)))
                         (cons right (%text-atom (subseq text split))))
                   environment emit))))
      ((and (not (logic-var-p resolved-left))
            (not (logic-var-p resolved-right)))
       (%unify-emit whole
                    (%text-atom (concatenate 'string (%atom-text resolved-left)
                                             (%atom-text resolved-right)))
                    environment emit))
      (t
       (%raise-instantiation-error environment operation
                                   "atom_concat/3 requires the whole atom or both parts")))))

(define-builtin (sub_atom atom before length after sub) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let* ((operation (%iso-atom "SUB_ATOM"))
         (resolved-atom (logic-substitute atom environment))
         (resolved-before (logic-substitute before environment))
         (resolved-length (logic-substitute length environment))
         (resolved-after (logic-substitute after environment))
         (resolved-sub (logic-substitute sub environment)))
    (%ensure-atom-value resolved-atom environment operation "sub_atom/5 atom")
    (%ensure-nonnegative-integer-or-variable resolved-before environment operation "before")
    (%ensure-nonnegative-integer-or-variable resolved-length environment operation "length")
    (%ensure-nonnegative-integer-or-variable resolved-after environment operation "after")
    (unless (or (logic-var-p resolved-sub) (%term-atom-p resolved-sub))
      (%raise-type-error "ATOM" resolved-sub environment operation
                         "sub_atom/5 subterm must be an atom"))
    (let* ((text (%atom-text resolved-atom))
           (size (length text)))
      (loop for candidate-before from 0 to size
            do (loop for candidate-length from 0 to (- size candidate-before)
                     for candidate-after = (- size candidate-before candidate-length)
                     do (%emit-pairs
                         (list (cons before candidate-before)
                               (cons length candidate-length)
                               (cons after candidate-after)
                               (cons sub (%text-atom
                                          (subseq text candidate-before
                                                  (+ candidate-before candidate-length)))))
                         environment emit))))))
