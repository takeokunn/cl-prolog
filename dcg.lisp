(in-package #:fx.prolog)

(defparameter *dcg-counter* 0)

(defun %fresh-dcg-var ()
  (intern (format nil "?S~D" (incf *dcg-counter*)) *package*))

(defun %thread-dcg-elements (elements s-in s-out element->goals)
  (if (null elements)
      (list `(= ,s-in ,s-out))
      (labels ((recurse (remaining current)
                 (let* ((element (car remaining))
                        (rest (cdr remaining))
                        (next (if rest (%fresh-dcg-var) s-out)))
                   (append (funcall element->goals element current next)
                           (when rest
                             (recurse rest next))))))
        (recurse elements s-in))))

(defun %dcg-body-form-tag-p (element tag)
  (and (consp element)
       (symbolp (car element))
       (string= (symbol-name (car element)) tag)))

(defun %dcg-transform-body-element (element s-in s-out)
  (cond
    ((%dcg-body-form-tag-p element "TERMINAL")
     (%thread-dcg-elements
      (cdr element) s-in s-out
      (lambda (terminal current next)
        (list `(dcg-token-match ,terminal ,current ,next)))))
    ((%dcg-body-form-tag-p element "BRACE")
     (list `(:when ,(cadr element)) `(= ,s-in ,s-out)))
    ((or (symbolp element) (consp element))
     (list (if (symbolp element)
               (list element s-in s-out)
               (append element (list s-in s-out)))))
    (t (error "DCG: unknown body element ~S" element))))

(defun %dcg-transform-body (body s-in s-out)
  (%thread-dcg-elements body s-in s-out #'%dcg-transform-body-element))

(defmacro def-dcg-rule (name &body body)
  (let ((s-in (gensym "?S-IN"))
        (s-out (gensym "?S-OUT")))
    `(def-rule (,name ,s-in ,s-out)
       ,@( %dcg-transform-body body s-in s-out))))

(defun phrase (rule-name input)
  (let ((solutions (query-prolog (%global-rulebase-view) (list rule-name input '?dcg-rest))))
    (first (mapcar (lambda (solution)
                      (logic-substitute '?dcg-rest solution))
                    solutions))))

(defun phrase-all (rule-name input)
  (mapcar (lambda (solution)
             (logic-substitute '?dcg-rest solution))
          (query-prolog (%global-rulebase-view) (list rule-name input '?dcg-rest))))
