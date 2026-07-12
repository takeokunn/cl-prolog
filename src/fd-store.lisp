(in-package #:cl-prolog)

(defstruct (fd-store (:constructor %make-fd-store (domains constraints)))
  (domains nil :type list :read-only t)
  (constraints nil :type list :read-only t))

(defstruct (fd-constraint (:constructor %make-fd-constraint (operator arguments)))
  operator
  (arguments nil :type list :read-only t))

(defparameter *fd-store* (%make-fd-store nil nil))

(defun %fd-domain (values)
  (sort (remove-duplicates (copy-list values)) #'<))

(defun %fd-interval (lower upper)
  (and (<= lower upper) (loop for value from lower to upper collect value)))

(defun %fd-domain-spec (term environment)
  (let ((resolved (logic-substitute term environment)))
    (cond
      ((and (%proper-list-p resolved)
            (= (length resolved) 3)
            (symbolp (second resolved))
            (string= (symbol-name (second resolved)) "..")
            (integerp (first resolved))
            (integerp (third resolved)))
       (%fd-interval (first resolved) (third resolved)))
      ((and (%proper-list-p resolved) (every #'integerp resolved))
       (%fd-domain resolved))
      (t
       (%raise-type-error "FINITE_DOMAIN" resolved environment
                          (%iso-atom "IN") "integer list or (Lower .. Upper) required")))))

(defun %fd-domain-of (store variable)
  (cdr (assoc variable (fd-store-domains store) :test #'eq)))

(defun %fd-put-domain (store variable domain)
  (%make-fd-store (acons variable domain
                         (remove variable (fd-store-domains store)
                                 :key #'car :test #'eq))
                  (fd-store-constraints store)))

(defun %fd-restrict-domain (store variable allowed)
  (let* ((current (%fd-domain-of store variable))
         (domain (if current (intersection current allowed) allowed)))
    (values (%fd-put-domain store variable (%fd-domain domain))
            (not (null domain)))))

(defun %fd-add-constraint (store operator arguments)
  (%make-fd-store (fd-store-domains store)
                  (cons (%make-fd-constraint operator arguments)
                        (fd-store-constraints store))))

(defun %fd-expression-value (expression environment)
  (let ((resolved (logic-substitute expression environment)))
    (labels ((evaluate (term)
               (cond
                 ((integerp term) (values term t))
                 ((logic-var-p term) (values nil nil))
                 ((and (%proper-list-p term) (= (length term) 3))
                  (multiple-value-bind (left leftp) (evaluate (second term))
                    (multiple-value-bind (right rightp) (evaluate (third term))
                      (if (and leftp rightp)
                          (values (case (%arithmetic-operator-key (first term))
                                    (:+ (+ left right))
                                    (:- (- left right))
                                    (:* (* left right))
                                    (otherwise
                                     (%raise-type-error "FD_EXPRESSION" term environment
                                                        (%iso-atom "CLPFD")
                                                        "only +, - and * are supported")))
                                  t)
                          (values nil nil)))))
                 (t (%raise-type-error "FD_EXPRESSION" term environment
                                       (%iso-atom "CLPFD")
                                       "integer, variable, or binary expression required")))))
      (evaluate resolved))))

(defun %fd-relation-true-p (operator left right)
  (ecase operator
    (|#=| (= left right))
    (|#\\=| (/= left right))
    (|#<| (< left right))
    (|#=<| (<= left right))
    (|#>| (> left right))
    (|#>=| (>= left right))))

(defun %fd-filter-variable (store variable operator constant variable-left-p)
  (let ((domain (%fd-domain-of store variable)))
    (if domain
        (%fd-restrict-domain
         store variable
         (remove-if-not (lambda (value)
                          (if variable-left-p
                              (%fd-relation-true-p operator value constant)
                              (%fd-relation-true-p operator constant value)))
                        domain))
        (values store t))))

(defun %fd-filter-variable-pair (store left right operator)
  (let ((left-domain (%fd-domain-of store left))
        (right-domain (%fd-domain-of store right)))
    (if (and left-domain right-domain)
        (let ((supported-left
                (remove-if-not
                 (lambda (left-value)
                   (some (lambda (right-value)
                           (%fd-relation-true-p operator left-value right-value))
                         right-domain))
                 left-domain)))
          (multiple-value-bind (left-store left-success-p)
              (%fd-restrict-domain store left supported-left)
            (if (not left-success-p)
                (values left-store nil)
                (let ((restricted-left (%fd-domain-of left-store left)))
                  (%fd-restrict-domain
                   left-store right
                   (remove-if-not
                    (lambda (right-value)
                      (some (lambda (left-value)
                              (%fd-relation-true-p operator left-value right-value))
                            restricted-left))
                    right-domain))))))
        (values store t))))

(defun %fd-propagate-relation (store constraint environment)
  (destructuring-bind (left right) (fd-constraint-arguments constraint)
    (multiple-value-bind (left-value left-ground-p)
        (%fd-expression-value left environment)
      (multiple-value-bind (right-value right-ground-p)
          (%fd-expression-value right environment)
        (let ((operator (fd-constraint-operator constraint))
              (resolved-left (logic-substitute left environment))
              (resolved-right (logic-substitute right environment)))
          (cond
            ((and left-ground-p right-ground-p)
             (values store (%fd-relation-true-p operator left-value right-value)))
            ((and (logic-var-p resolved-left) right-ground-p)
             (%fd-filter-variable store resolved-left operator right-value t))
            ((and left-ground-p (logic-var-p resolved-right))
             (%fd-filter-variable store resolved-right operator left-value nil))
            ((and (logic-var-p resolved-left) (logic-var-p resolved-right))
             (%fd-filter-variable-pair store resolved-left resolved-right operator))
            (t (values store t))))))))

(defun %fd-distinct-assignment-p (variables store used)
  "True when VARIABLES have a distinct assignment compatible with STORE."
  (if (endp variables)
      t
      (some (lambda (value)
              (and (not (member value used))
                   (%fd-distinct-assignment-p (rest variables) store
                                              (cons value used))))
            (%fd-domain-of store (first variables)))))

(defun %fd-propagate-all-different (store constraint environment)
  (let* ((terms (mapcar (lambda (term) (logic-substitute term environment))
                        (fd-constraint-arguments constraint)))
         (ground (remove-if-not #'integerp terms)))
    (if (/= (length ground) (length (remove-duplicates ground)))
        (values store nil)
        (loop with current = store
              for term in terms
              when (logic-var-p term)
                do (multiple-value-bind (next successp)
                       (%fd-restrict-domain current term
                                            (set-difference (%fd-domain-of current term) ground))
                     (unless successp (return (values next nil)))
                     (setf current next))
              finally
                 (return
                   (values current
                           (%fd-distinct-assignment-p
                            (remove-if-not #'logic-var-p terms)
                            current ground)))))))

(defun %fd-propagate (store environment)
  (loop with current = store
        repeat (1+ (length (fd-store-constraints store)))
        do (let ((before (fd-store-domains current)))
             (dolist (constraint (fd-store-constraints current))
               (multiple-value-bind (next successp)
                   (if (eq (fd-constraint-operator constraint) 'all_different)
                       (%fd-propagate-all-different current constraint environment)
                       (%fd-propagate-relation current constraint environment))
                 (unless successp (return-from %fd-propagate (values next nil)))
                 (setf current next)))
             (when (equal before (fd-store-domains current))
               (return (values current t))))
        finally (return (values current t))))
