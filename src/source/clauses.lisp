(in-package #:cl-prolog)

(defun %source-term-clause (term)
  (cond
    ((symbolp term) (make-clause (list term)))
    ((and (consp term) (eq (first term) '-->) (= (length term) 3))
     (%expand-prolog-dcg-clause (second term) (third term)))
    ((and (consp term)
          (eq (first term) (%prolog-symbol ":-"))
          (= (length term) 3))
     (let ((head (second term)))
       (unless (or (symbolp head) (consp head))
         (error "Invalid Prolog clause head ~S." head))
       (make-clause (if (symbolp head) (list head) head)
                    (%body-goals (third term)))))
    ((consp term) (make-clause term))
    (t (error "Invalid consultable Prolog term ~S." term))))

(defun %insert-source-clause! (rulebase term module)
  (let ((clause (%source-term-clause term)))
    (multiple-value-bind (predicate arity)
        (values (first (clause-head clause)) (length (rest (clause-head clause))))
      (module-registry-ensure-definition-allowed
       (rulebase-module-registry rulebase) module predicate arity)
      (unless (%rulebase-predicate-property rulebase predicate arity module)
        (%set-rulebase-predicate-property! rulebase predicate arity :static module)
        (%record-source-predicate-property!
         module predicate arity :static)))
    (rulebase-insert-clause! rulebase clause :module module
                             :source *current-prolog-source*)))

