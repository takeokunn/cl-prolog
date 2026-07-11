(in-package #:cl-prolog)

(define-builtin (member item list-term) (rulebase environment depth emit)
  (let ((elements (logic-substitute list-term environment)))
    (when (%proper-list-p elements)
      (dolist (element elements)
        (%unify-emit item element environment emit)))))

(define-builtin (append left right result) (rulebase environment depth emit)
  (let ((left-value (logic-substitute left environment))
        (result-value (logic-substitute result environment)))
    (cond
      ((%proper-list-p left-value)
       (%unify-emit result
                    (append left-value (logic-substitute right environment))
                    environment emit))
      ((%proper-list-p result-value)
       (loop for split from 0 to (length result-value)
             do (multiple-value-bind (extended ok)
                    (unify left (subseq result-value 0 split) environment)
                  (when ok
                    (%unify-emit right (nthcdr split result-value)
                                 extended emit))))))))

(define-builtin (reverse forward backward) (rulebase environment depth emit)
  (let ((forward-value (logic-substitute forward environment))
        (backward-value (logic-substitute backward environment)))
    (cond
      ((%proper-list-p forward-value)
       (%unify-emit backward (reverse forward-value) environment emit))
      ((%proper-list-p backward-value)
       (%unify-emit forward (reverse backward-value) environment emit)))))

(define-builtin (length list-term length-term) (rulebase environment depth emit)
  (let ((list-value (logic-substitute list-term environment))
        (length-value (logic-substitute length-term environment)))
    (cond
      ((%proper-list-p list-value)
       (%unify-emit length-term (length list-value) environment emit))
      ((typep length-value '(integer 0))
       (%unify-emit list-term
                    (loop repeat length-value collect (fresh-logic-variable))
                    environment emit)))))
