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
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((visit (tail current-environment)
               (let ((resolved (%walk-term tail current-environment)))
                 (unless (and (consp resolved) (gethash resolved seen))
                   (when (consp resolved)
                     (setf (gethash resolved seen) t))
                   (let ((head (fresh-logic-variable "?MEMBER-HEAD"))
                         (rest (fresh-logic-variable "?MEMBER-TAIL")))
                     (%unify-sequence
                      (list (cons tail (cons head rest)))
                      current-environment
                      (lambda (extended)
                        (%unify-emit item head extended emit)
                        (visit rest extended))))))))
      (visit list-term environment))))

(define-builtin (memberchk item list-term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((visit (tail current-environment)
               (let ((resolved (%walk-term tail current-environment)))
                 (unless (and (consp resolved) (gethash resolved seen))
                   (when (consp resolved)
                     (setf (gethash resolved seen) t))
                   (let ((head (fresh-logic-variable "?MEMBERCHK-HEAD"))
                         (rest (fresh-logic-variable "?MEMBERCHK-TAIL")))
                     (%unify-sequence
                      (list (cons tail (cons head rest)))
                      current-environment
                      (lambda (extended)
                        (multiple-value-bind (matched ok)
                            (unify item head extended)
                          (if ok
                              (funcall emit matched)
                              (visit rest extended))))))))))
      (visit list-term environment))))

(define-builtin (select item list-term rest-term)
    (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (labels ((visit (tail prefix current-environment seen)
             (let ((resolved (%walk-term tail current-environment)))
               (unless (and (consp resolved) (gethash resolved seen))
                 (let ((head (fresh-logic-variable "?SELECT-HEAD"))
                       (rest (fresh-logic-variable "?SELECT-TAIL")))
                   (%unify-sequence
                    (list (cons tail (cons head rest))
                          (cons item head)
                          (cons rest-term (append (reverse prefix) rest)))
                    current-environment emit)
                   (when (consp resolved)
                     (setf (gethash resolved seen) t))
                   (%unify-sequence
                    (list (cons tail (cons head rest)))
                    current-environment
                    (lambda (extended)
                      (visit rest (cons head prefix) extended seen))))))))
    (visit list-term '() environment (make-hash-table :test #'eq))))

(defun %list-index (index list-term item offset environment emit operation)
  "Implement NTH0/3 and NTH1/3 with OFFSET as the first valid index."
  (let ((resolved-index (%walk-term index environment))
        (seen (make-hash-table :test #'eq)))
    (unless (logic-var-p resolved-index)
      (unless (integerp resolved-index)
        (%raise-type-error "INTEGER" resolved-index environment operation
                           "The list index must be an integer"))
      (when (< resolved-index offset)
        (%raise-domain-error (if (zerop offset)
                                 "NOT_LESS_THAN_ZERO"
                                 "NOT_LESS_THAN_ONE")
                             resolved-index
                             environment operation
                             "The list index is below the valid range")))
    (labels ((visit (tail position current-environment)
               (let ((resolved (%walk-term tail current-environment)))
                 (unless (and (consp resolved) (gethash resolved seen))
                   (when (consp resolved)
                     (setf (gethash resolved seen) t))
                   (let ((head (fresh-logic-variable "?NTH-HEAD"))
                         (rest (fresh-logic-variable "?NTH-TAIL")))
                     (%unify-sequence
                      (list (cons tail (cons head rest)))
                      current-environment
                      (lambda (extended)
                        (when (or (logic-var-p resolved-index)
                                  (= resolved-index position))
                          (%unify-sequence
                           (list (cons index position) (cons item head))
                           extended emit))
                        (when (or (logic-var-p resolved-index)
                                  (> resolved-index position))
                          (visit rest (1+ position) extended)))))))))
      (visit list-term offset environment))))

(define-builtin (nth0 index list-term item) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (%list-index index list-term item 0 environment emit (%iso-atom "NTH0")))

(define-builtin (nth1 index list-term item) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (%list-index index list-term item 1 environment emit (%iso-atom "NTH1")))

(define-builtin (last list-term item) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((visit (tail current-environment)
               (let ((resolved (%walk-term tail current-environment)))
                 (unless (and (consp resolved) (gethash resolved seen))
                   (when (consp resolved)
                     (setf (gethash resolved seen) t))
                   (let ((head (fresh-logic-variable "?LAST-HEAD"))
                         (rest (fresh-logic-variable "?LAST-TAIL")))
                     (%unify-sequence
                      (list (cons tail (cons head rest)) (cons rest nil)
                            (cons item head))
                      current-environment emit)
                     (%unify-sequence
                      (list (cons tail (cons head rest)))
                      current-environment
                      (lambda (extended)
                        (visit rest extended))))))))
      (visit list-term environment))))

(define-builtin (is_list list-term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let ((seen (make-hash-table :test #'eq)))
    (loop for tail = (%walk-term list-term environment) then (%walk-term (cdr tail)
                                                                        environment)
          do (cond
               ((null tail) (return (funcall emit environment)))
               ((or (atom tail) (gethash tail seen)) (return nil))
               (t (setf (gethash tail seen) t))))))

(define-builtin (append left right result) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((join (left-tail result-tail current-environment)
               (let ((resolved (%walk-term left-tail current-environment)))
                 (unless (and (consp resolved) (gethash resolved seen))
                   (when (consp resolved)
                     (setf (gethash resolved seen) t))
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
                        (join left-rest result-rest extended))))))))
      (join left result environment))))

(define-builtin (reverse forward backward) (rulebase environment depth emit)
  (let ((forward-value (logic-substitute forward environment))
        (backward-value (logic-substitute backward environment)))
    (cond
      ((%proper-list-p forward-value)
       (%unify-emit backward (reverse forward-value) environment emit))
      ((%proper-list-p backward-value)
       (%unify-emit forward (reverse backward-value) environment emit)))))

(define-builtin (length list-term length-term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let ((length-value (%walk-term length-term environment))
        (operation (%iso-atom "LENGTH"))
        (seen (make-hash-table :test #'eq)))
    (unless (logic-var-p length-value)
      (unless (integerp length-value)
        (%raise-type-error "INTEGER" length-value environment operation
                           "length/2 length must be an integer"))
      (when (minusp length-value)
        (%raise-domain-error "NOT_LESS_THAN_ZERO" length-value
                             environment operation
                             "length/2 length must not be negative")))
    (labels ((measure (tail count current-environment)
               (let ((resolved-tail (%walk-term tail current-environment)))
                 (unless (and (consp resolved-tail)
                              (gethash resolved-tail seen))
                   (multiple-value-bind (closed closedp)
                       (unify tail nil current-environment)
                     (when closedp
                       (%unify-emit length-term count closed emit)))
                   (when (or (logic-var-p length-value)
                             (< count length-value))
                     (when (consp resolved-tail)
                       (setf (gethash resolved-tail seen) t))
                     (let ((head (fresh-logic-variable "?LENGTH-HEAD"))
                           (rest (fresh-logic-variable "?LENGTH-TAIL")))
                       (%unify-sequence
                        (list (cons tail (cons head rest)))
                        current-environment
                        (lambda (extended)
                          (measure rest (1+ count) extended)))))))))
      (measure list-term 0 environment))))
