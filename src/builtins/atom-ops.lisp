;;;; Atom-text builtins: atom_length/2, atom_concat/3, sub_atom/5.

(in-package #:cl-prolog)

(define-iso-builtin (atom_length atom length) "ATOM_LENGTH"
  (%ensure-atom-value resolved-atom environment operation "atom_length/2 atom")
  (%ensure-nonnegative-integer-or-variable
   resolved-length environment operation "atom_length/2 length")
  (%check-atom-text-limit resolved-atom environment operation)
  (%unify-emit length (length (%atom-text resolved-atom)) environment emit))

(define-iso-builtin (atom_concat left right whole) "ATOM_CONCAT"
  (dolist (value (list resolved-left resolved-right resolved-whole))
      (unless (or (logic-var-p value) (%term-atom-p value))
        (%raise-type-error "ATOM" value environment operation
                           "atom_concat/3 arguments must be atoms")))
    (dolist (value (list resolved-left resolved-right resolved-whole))
      (unless (logic-var-p value)
        (%check-atom-text-limit value environment operation)))
    (cond
      ((not (logic-var-p resolved-whole))
       (let* ((whole-text (%atom-text resolved-whole))
              (whole-size (length whole-text)))
         (cond
           ((and (not (logic-var-p resolved-left))
                 (not (logic-var-p resolved-right)))
            (let* ((left-text (%atom-text resolved-left))
                   (right-text (%atom-text resolved-right))
                   (left-size (length left-text))
                   (right-size (length right-text)))
              (when (and (= (+ left-size right-size) whole-size)
                         (string= left-text whole-text :end2 left-size)
                         (string= right-text whole-text :start2 left-size))
                (funcall emit environment))))
           ((not (logic-var-p resolved-left))
            (let* ((left-text (%atom-text resolved-left))
                   (left-size (length left-text)))
              (when (and (<= left-size whole-size)
                         (string= left-text whole-text :end2 left-size))
                (%unify-emit
                 right
                 (%text-atom (subseq whole-text left-size) environment operation)
                 environment emit))))
           ((not (logic-var-p resolved-right))
            (let* ((right-text (%atom-text resolved-right))
                   (right-size (length right-text))
                   (split (- whole-size right-size)))
              (when (and (not (minusp split))
                         (string= right-text whole-text :start2 split))
                (%unify-emit
                 left
                 (%text-atom (subseq whole-text 0 split) environment operation)
                 environment emit))))
           (t
            (%check-resource-limit
             (1+ whole-size) *max-prolog-derived-atom-candidates*
             "ATOM_CANDIDATES" environment operation
             "atom_concat/3 candidate count exceeds the configured limit")
            (loop for split from 0 to whole-size
                  do (%term-unify-sequence
                      (list
                       (cons left
                             (%text-atom
                              (subseq whole-text 0 split)
                              environment operation))
                       (cons right
                             (%text-atom
                              (subseq whole-text split)
                              environment operation)))
                      environment emit))))))
      ((and (not (logic-var-p resolved-left))
            (not (logic-var-p resolved-right)))
       (let* ((left-text (%atom-text resolved-left))
              (right-text (%atom-text resolved-right))
              (combined-size (+ (length left-text) (length right-text))))
         (%check-resource-limit
          combined-size *max-prolog-quoted-lexeme-length* "ATOM_LENGTH"
          environment operation "derived atom exceeds the configured length limit")
         (%unify-emit
          whole
          (%text-atom
           (concatenate 'string left-text right-text) environment operation)
          environment emit)))
      (t
       (%raise-instantiation-error environment operation
                                   "atom_concat/3 requires the whole atom or both parts"))))

(define-iso-builtin (sub_atom atom before length after sub) "SUB_ATOM"
  (%ensure-atom-value resolved-atom environment operation "sub_atom/5 atom")
    (%ensure-nonnegative-integer-or-variable
     resolved-before environment operation "before")
    (%ensure-nonnegative-integer-or-variable
     resolved-length environment operation "length")
    (%ensure-nonnegative-integer-or-variable
     resolved-after environment operation "after")
    (unless (or (logic-var-p resolved-sub) (%term-atom-p resolved-sub))
      (%raise-type-error "ATOM" resolved-sub environment operation
                         "sub_atom/5 subterm must be an atom"))
    (%check-atom-text-limit resolved-atom environment operation)
    (unless (logic-var-p resolved-sub)
      (%check-atom-text-limit resolved-sub environment operation))
    (let* ((text (%atom-text resolved-atom))
           (size (length text))
           (sub-bound-p (not (logic-var-p resolved-sub)))
           (sub-text (and sub-bound-p (%atom-text resolved-sub)))
           (sub-size (and sub-bound-p (length sub-text)))
           (fixed-length
             (cond
               ((not (logic-var-p resolved-length)) resolved-length)
               (sub-bound-p sub-size)
               ((and (not (logic-var-p resolved-before))
                     (not (logic-var-p resolved-after)))
                (- size resolved-before resolved-after))))
           (fixed-before
             (cond
               ((not (logic-var-p resolved-before)) resolved-before)
               ((and (not (logic-var-p resolved-after))
                     (integerp fixed-length))
                (- size resolved-after fixed-length)))))
      (labels ((bound-integer-matches-p (value candidate)
                 (or (logic-var-p value) (= value candidate)))
               (emit-candidate (candidate-before candidate-length)
                 (let ((candidate-after
                         (- size candidate-before candidate-length)))
                   (when
                       (and (not (minusp candidate-before))
                            (not (minusp candidate-length))
                            (not (minusp candidate-after))
                            (bound-integer-matches-p
                             resolved-before candidate-before)
                            (bound-integer-matches-p
                             resolved-length candidate-length)
                            (bound-integer-matches-p
                             resolved-after candidate-after)
                            (or (not sub-bound-p)
                                (and (= sub-size candidate-length)
                                     (string=
                                      sub-text text
                                      :start2 candidate-before
                                      :end2 (+ candidate-before
                                               candidate-length)))))
                     (%term-unify-sequence
                      (list
                       (cons before candidate-before)
                       (cons length candidate-length)
                       (cons after candidate-after)
                       (cons sub
                             (if sub-bound-p
                                 resolved-sub
                                 (%text-atom
                                  (subseq
                                   text candidate-before
                                   (+ candidate-before candidate-length))
                                  environment operation))))
                      environment emit)))))
        (cond
          ((and (integerp fixed-before) (integerp fixed-length))
           (emit-candidate fixed-before fixed-length))
          ((integerp fixed-before)
           (loop for candidate-length from 0 to (- size fixed-before)
                 do (emit-candidate fixed-before candidate-length)))
          ((integerp fixed-length)
           (loop for candidate-before from 0 to (- size fixed-length)
                 do (emit-candidate candidate-before fixed-length)))
          ((not (logic-var-p resolved-after))
           (loop for candidate-before from 0 to (- size resolved-after)
                 for candidate-length = (- size resolved-after candidate-before)
                 do (emit-candidate candidate-before candidate-length)))
          (t
           (%check-resource-limit
            (/ (* (1+ size) (+ size 2)) 2)
            *max-prolog-derived-atom-candidates*
            "ATOM_CANDIDATES" environment operation
            "sub_atom/5 candidate count exceeds the configured limit")
           (loop for candidate-before from 0 to size
                 do (loop
                      for candidate-length from 0 to (- size candidate-before)
                      do (emit-candidate
                          candidate-before candidate-length))))))))
