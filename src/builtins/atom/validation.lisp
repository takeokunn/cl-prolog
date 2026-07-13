;;;; Atom, character, and numeric text conversion builtins.

(in-package #:cl-prolog)

(defun %ensure-atom-value (value environment operation argument)
  (when (logic-var-p value)
    (%raise-instantiation-error environment operation
                                (format nil "~A must be instantiated" argument)))
  (unless (%term-atom-p value)
    (%raise-type-error "ATOM" value environment operation
                       (format nil "~A must be an atom" argument)))
  value)

(defun %ensure-nonnegative-integer-or-variable (value environment operation argument)
  (unless (logic-var-p value)
    (unless (integerp value)
      (%raise-type-error "INTEGER" value environment operation
                         (format nil "~A must be an integer" argument)))
    (when (minusp value)
      (%raise-domain-error "NOT_LESS_THAN_ZERO" value environment operation
                           (format nil "~A must not be negative" argument))))
  value)

(defun %ensure-proper-instantiated-list (value environment operation argument)
  (let ((visited (make-hash-table :test #'eq))
        (tail value))
    (loop
      (cond
        ((null tail) (return value))
        ((logic-var-p tail)
         (%raise-instantiation-error
          environment operation (format nil "~A must be instantiated" argument)))
        ((not (consp tail))
         (%raise-type-error "LIST" value environment operation
                            (format nil "~A must be a proper list" argument)))
        ((gethash tail visited)
         (%raise-type-error "LIST" value environment operation
                            (format nil "~A must be a finite proper list" argument)))
        (t
         (setf (gethash tail visited) t)
         (when (logic-var-p (car tail))
           (%raise-instantiation-error
            environment operation (format nil "~A must be instantiated" argument)))
         (setf tail (cdr tail)))))))

