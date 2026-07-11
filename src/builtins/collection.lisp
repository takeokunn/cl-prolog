(in-package #:cl-prolog)

(defun %strip-existential-quantifiers (goal)
  "Return GOAL without leading (^ VARIABLE GOAL) forms and their variables."
  (let ((variables '()))
    (loop while (and (%proper-list-p goal)
                     (= (length goal) 3)
                     (and (symbolp (first goal))
                          (string= (symbol-name (first goal)) "^"))
                     (logic-var-p (second goal)))
          do (pushnew (second goal) variables :test #'eq)
             (setf goal (third goal)))
    (values goal (nreverse variables))))

(defun %collect-template-solutions (template goal rulebase environment depth)
  "Collect copied TEMPLATE instances for every proof of GOAL."
  (let ((solutions '()))
    (%prove-bindings/k
     (logic-substitute goal environment)
     rulebase environment depth
     (lambda (extended)
       (push (%freshen-term (logic-substitute template extended)
                            (make-hash-table :test #'eq))
             solutions)))
    (nreverse solutions)))

(defun %bagof-free-variables (template goal existential-variables)
  "Return the free variables by which BAGOF and SETOF group solutions."
  (let ((template-variables (%collect-variables template)))
    (remove-if (lambda (variable)
                 (or (member variable template-variables :test #'eq)
                     (member variable existential-variables :test #'eq)))
               (%collect-variables goal))))

(defun %collect-grouped-solutions (template goal free-variables
                                    rulebase environment depth)
  "Collect (KEY . TEMPLATE) pairs while preserving proof order."
  (let ((solutions '()))
    (%prove-bindings/k
     (logic-substitute goal environment)
     rulebase environment depth
     (lambda (extended)
       (let ((table (make-hash-table :test #'eq)))
         (push (cons (%freshen-term
                      (mapcar (lambda (variable)
                                (logic-substitute variable extended))
                              free-variables)
                      table)
                     (%freshen-term (logic-substitute template extended) table))
               solutions))))
    (nreverse solutions)))

(defun %partition-solution-groups (solutions)
  "Partition (KEY . TEMPLATE) SOLUTIONS by structurally equal keys."
  (let ((groups '()))
    (dolist (solution solutions (nreverse groups))
      (let ((group (find (car solution) groups :key #'car :test #'equal)))
        (if group
            (setf (cdr group) (nconc (cdr group) (list (cdr solution))))
            (push (list (car solution) (cdr solution)) groups))))))

(defun %emit-solution-group (free-variables key values bag environment emit)
  "Unify one grouped KEY and VALUES with the caller and emit on success."
  (let ((extended environment)
        (ok t))
    (loop for variable in free-variables
          for value in key
          while ok
          do (multiple-value-setq (extended ok)
               (unify variable value extended)))
    (when ok
      (%unify-emit bag values extended emit))))

(defun %prolog-term< (left right)
  "Compare terms using the standard Prolog order."
  (minusp (%compare-terms left right)))

(defun %emit-bagof-solutions (template quantified-goal bag setp
                              rulebase environment depth emit)
  (multiple-value-bind (goal existential-variables)
      (%strip-existential-quantifiers quantified-goal)
    (let* ((free-variables
             (%bagof-free-variables template goal existential-variables))
           (solutions
             (%collect-grouped-solutions template goal free-variables
                                         rulebase environment depth)))
      (dolist (group (%partition-solution-groups solutions))
        (let ((values (cdr group)))
          (when setp
            (setf values (sort (remove-duplicates values :test #'equal)
                               #'%prolog-term<)))
          (%emit-solution-group free-variables (car group) values bag
                                environment emit))))))

(define-builtin (findall template goal bag) (rulebase environment depth emit)
  (%unify-emit bag
               (%collect-template-solutions template goal rulebase environment depth)
               environment emit))

(define-builtin (bagof template goal bag) (rulebase environment depth emit)
  (%emit-bagof-solutions template goal bag nil rulebase environment depth emit))

(define-builtin (setof template goal bag) (rulebase environment depth emit)
  (%emit-bagof-solutions template goal bag t rulebase environment depth emit))

(defun %resolved-collection-list (term environment predicate)
  (let ((resolved (logic-substitute term environment)))
    (cond
      ((logic-var-p resolved)
       (%raise-instantiation-error environment predicate
                                   "The input list must be instantiated"))
      ((not (%proper-list-p resolved))
       (%raise-type-error "LIST" resolved environment predicate
                          "The input must be a proper list"))
      (t resolved))))

(defun %key-value-term-p (term)
  (and (%proper-list-p term)
       (= (length term) 3)
       (symbolp (first term))
       (string= (symbol-name (first term)) "-")))

(defun %keysort-key (pair environment)
  (unless (%key-value-term-p pair)
    (%raise-type-error "PAIR" pair environment (%iso-atom "KEYSORT")
                       "KEYSORT/2 expects Key-Value pairs"))
  (second pair))

(define-builtin (sort input sorted) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let* ((values (%resolved-collection-list input environment (%iso-atom "SORT")))
         (ordered (stable-sort (remove-duplicates values :test #'equal)
                               #'%prolog-term<)))
    (%unify-emit sorted ordered environment emit)))

(define-builtin (keysort input sorted) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let ((pairs (%resolved-collection-list input environment
                                          (%iso-atom "KEYSORT"))))
    (dolist (pair pairs)
      (%keysort-key pair environment))
    (%unify-emit sorted
                 (stable-sort (copy-list pairs) #'%prolog-term<
                              :key (lambda (pair)
                                     (%keysort-key pair environment)))
                 environment emit)))

(define-builtin (:when test &rest variables) (rulebase environment depth emit)
  ;; TEST receives the solved value of each of VARIABLES.  The DSL compiles
  ;; (:when EXPR) guards into such functions; hand-written queries must pass
  ;; a function object too.
  (unless (functionp test)
    (%invalid-goal (list* :when test variables)
                   ":WHEN needs a guard function, got ~S (use the PROLOG ~
                    macro to compile expression guards)" test))
  (when (apply test
               (mapcar (lambda (variable)
                         (logic-substitute variable environment))
                       variables))
    (funcall emit environment)))
