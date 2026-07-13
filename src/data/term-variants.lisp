(in-package #:cl-prolog)

(defun %canonicalize-variant (term)
  "Rename TERM's variables by first occurrence, preserving sharing."
  (let ((variables (make-hash-table :test #'eq))
        (copies (make-hash-table :test #'eq))
        (next-index 0))
    (labels ((canonicalize (node)
               (cond
                 ((logic-var-p node)
                  (or (gethash node variables)
                      (setf (gethash node variables)
                            (list +variant-variable-marker+
                                  (prog1 next-index (incf next-index))))))
                 ((consp node)
                  (or (gethash node copies)
                      (let ((copy (cons nil nil)))
                        (setf (gethash node copies) copy
                              (car copy) (canonicalize (car node))
                              (cdr copy) (canonicalize (cdr node)))
                        copy)))
                 (t node))))
      (canonicalize term))))

(defun %instantiate-variant (term)
  "Replace canonical variable markers in TERM with fresh logic variables."
  (let ((variables (make-hash-table :test #'equal)))
    (labels ((instantiate (node)
               (cond
                 ((and (consp node)
                       (eq (first node) +variant-variable-marker+)
                       (consp (rest node))
                       (null (cddr node)))
                  (or (gethash node variables)
                      (setf (gethash node variables)
                            (fresh-logic-variable "?TABLE"))))
                 ((consp node)
                  (cons (instantiate (car node))
                        (instantiate (cdr node))))
                 (t node))))
      (instantiate term))))
