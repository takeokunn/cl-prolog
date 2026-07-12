(in-package #:cl-prolog)

(defun %fd-emit (store environment emit)
  (let ((*fd-store* store))
    (funcall emit environment)))

(defun %fd-variable-terms (term environment &optional (context (%iso-atom "CLPFD")))
  (let ((resolved (logic-substitute term environment)))
    (cond ((logic-var-p resolved) (list resolved))
          ((and (%proper-list-p resolved)
                (every (lambda (item) (logic-var-p (logic-substitute item environment))) resolved))
           (mapcar (lambda (item) (logic-substitute item environment)) resolved))
          (t (%raise-type-error "VARIABLE_OR_VARIABLE_LIST" resolved environment
                                context "unbound variable(s) required")))))

(defun %fd-domain-terms (term environment context)
  "Return the variables and integers accepted by IN/2 and INS/2."
  (let ((resolved (logic-substitute term environment)))
    (cond ((or (logic-var-p resolved) (integerp resolved)) (list resolved))
          ((and (%proper-list-p resolved)
                (every (lambda (item)
                         (let ((value (logic-substitute item environment)))
                           (or (logic-var-p value) (integerp value))))
                       resolved))
           (mapcar (lambda (item) (logic-substitute item environment)) resolved))
          (t (%raise-type-error "VARIABLE_INTEGER_OR_LIST" resolved environment
                                context "variable(s) and integer(s) required")))))

(defun %fd-post (operator arguments environment emit)
  (let ((left (logic-substitute (first arguments) environment))
        (right (logic-substitute (second arguments) environment)))
    (cond
      ((and (eq operator '|#=|)
            (or (and (logic-var-p left) (integerp right))
                (and (integerp left) (logic-var-p right))))
       (%constraint-unify-emit left right environment emit))
      (t
       (multiple-value-bind (store successp)
           (%fd-propagate (%fd-add-constraint *fd-store* operator arguments) environment)
         (when successp (%fd-emit store environment emit)))))))

(defmacro define-fd-relation (name)
  `(define-builtin (,name left right) (rulebase environment depth emit)
     (%fd-post ',name (list left right) environment emit)))

(defmacro %define-fd-domain-relation (name context)
  `(define-builtin (,name variables domain-spec)
     (rulebase environment depth emit)
     (%fd-constrain-domain variables domain-spec environment emit
                            (%iso-atom ,context))))

(%define-fd-domain-relation in "IN")

(%define-fd-domain-relation ins "INS")

(define-fd-relation |#=|)
(define-fd-relation |#\\=|)
(define-fd-relation |#<|)
(define-fd-relation |#=<|)
(define-fd-relation |#>|)
(define-fd-relation |#>=|)

(define-builtin (all_different variables) (rulebase environment depth emit)
  (%fd-post 'all_different
            (%fd-domain-terms variables environment (%iso-atom "ALL_DIFFERENT"))
            environment emit))

(defun %fd-label-options (options environment)
  (let ((resolved (logic-substitute options environment)))
    (unless (and (%proper-list-p resolved)
                 (every (lambda (option)
                          (and (symbolp option)
                               (member (symbol-name option) '("FF" "UP" "DOWN")
                                       :test #'string=)))
                        resolved))
      (%raise-domain-error "LABELING_OPTIONS" resolved environment
                           (%iso-atom "LABELING") "expected ff, up, or down"))
    resolved))

(defun %fd-option-p (name options)
  (find name options :key #'symbol-name :test #'string=))

(defun %fd-select-variable (variables store options)
  (if (%fd-option-p "FF" options)
      (car (sort (copy-list variables) #'<
                 :key (lambda (variable) (length (%fd-domain-of store variable)))))
      (first variables)))

(defun %fd-label (variables store environment options emit
                  &optional (context (%iso-atom "LABELING")))
  (let ((pending (remove-if-not #'logic-var-p
                                (mapcar (lambda (term) (logic-substitute term environment)) variables))))
    (if (null pending)
        (%fd-emit store environment emit)
        (let* ((variable (%fd-select-variable pending store options))
               (domain (%fd-domain-of store variable)))
          (unless domain
            (%raise-instantiation-error environment context
                                        "every labeling variable needs a finite domain"))
          (dolist (value (if (%fd-option-p "DOWN" options) (reverse domain) domain))
            (let ((next-environment (unify variable value environment)))
              (when next-environment
                (multiple-value-bind (next-store successp)
                    (%fd-propagate store next-environment)
                  (when successp
                    (let ((*fd-store* next-store))
                      (%fd-label pending next-store next-environment options emit context)))))))))))

(define-builtin (labeling options variables) (rulebase environment depth emit)
  (%fd-label (%fd-variable-terms variables environment)
             *fd-store* environment (%fd-label-options options environment) emit))

(define-builtin (indomain variable) (rulebase environment depth emit)
  (let ((context (%iso-atom "INDOMAIN")))
    (%fd-label (%fd-variable-terms variable environment context)
               *fd-store* environment nil emit context)))
