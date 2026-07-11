(in-package #:cl-prolog)

(defun %unify-sequence (pairs environment continuation)
  "Unify PAIRS from left to right, then invoke CONTINUATION."
  (if (null pairs)
      (funcall continuation environment)
      (multiple-value-bind (extended ok)
          (unify (caar pairs) (cdar pairs) environment)
        (when ok
          (%unify-sequence (cdr pairs) extended continuation)))))

(define-builtin (member item list-term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (labels ((visit (tail current-environment)
             (let ((head (fresh-logic-variable "?MEMBER-HEAD"))
                   (rest (fresh-logic-variable "?MEMBER-TAIL")))
               (%unify-sequence
                (list (cons tail (cons head rest)))
                current-environment
                (lambda (extended)
                  (%unify-emit item head extended emit)
                  (visit rest extended))))))
    (visit list-term environment)))

(define-builtin (append left right result) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (labels ((join (left-tail result-tail current-environment)
             ;; append([], Ys, Ys).
             (%unify-sequence
              (list (cons left-tail nil) (cons right result-tail))
              current-environment emit)
             ;; append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).
             (let ((head (fresh-logic-variable "?APPEND-HEAD"))
                   (left-rest (fresh-logic-variable "?APPEND-LEFT"))
                   (result-rest (fresh-logic-variable "?APPEND-RESULT")))
               (%unify-sequence
                (list (cons left-tail (cons head left-rest))
                      (cons result-tail (cons head result-rest)))
                current-environment
                (lambda (extended)
                  (join left-rest result-rest extended))))))
    (join left result environment)))

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
