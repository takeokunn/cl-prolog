(in-package #:cl-prolog.tests)

(defvar *observed-table-session* nil)

(defun capture-prolog-condition (thunk)
  (handler-case
      (progn
        (funcall thunk)
        (error "Expected a PROLOG-EXCEPTION"))
    (prolog-exception (condition)
      condition)))
