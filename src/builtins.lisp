;;;; Builtin goal solvers.
;;;;
;;;; Every builtin is declared with DEFINE-BUILTIN and follows the engine's
;;;; CPS contract: emit each solution environment as it is found, never
;;;; collect intermediate result lists.

(in-package #:fx.prolog)

(defun %unify-emit (left right environment emit)
  "EMIT the extension of ENVIRONMENT that unifies LEFT with RIGHT, if any."
  (multiple-value-bind (extended ok)
      (unify left right environment)
    (when ok
      (funcall emit extended))))

(defun %proper-list-p (value)
  "True when VALUE is a fully-instantiated proper list."
  (cond
    ((null value) t)
    ((consp value) (%proper-list-p (cdr value)))
    (t nil)))

(defun %entry-head (clause)
  (clause-head clause))

(defun %entry-body-term (clause)
  (if (null (clause-body clause))
      'true
      (let ((body (clause-body clause)))
        (cond
          ((null body) 'true)
          ((null (rest body)) (first body))
          (t (cons 'and body))))))

(defun %freshen-dynamic-clause (clause)
  (%freshen-clause clause))

(defun %builtin-predicate-p (predicate)
  (and (symbolp predicate) (gethash predicate *builtin-solvers*)))

(defun %ensure-dynamic-predicate (predicate goal)
  (when (%builtin-predicate-p predicate)
    (%invalid-goal goal "builtin predicate ~S cannot be inspected or modified" predicate)))

(defun %clause-term-entry (term goal)
  "Convert a substituted dynamic clause TERM to a freshly renamed entry."
  (cond
    ((and (%proper-list-p term) (eq (first term) ':-))
     (unless (and (consp (rest term))
                  (%goal-form-p (second term)))
       (%invalid-goal goal "a rule must have shape (:- (PREDICATE . ARGS) GOAL...)"))
     (%ensure-dynamic-predicate (first (second term)) goal)
     (let ((body (cddr term)))
       (unless (every #'%goal-form-p (mapcar #'%ensure-goal-form body))
         (%invalid-goal goal "every rule body element must be a callable goal"))
       (%freshen-clause (make-clause (second term) body))))
    ((%goal-form-p term)
     (%ensure-dynamic-predicate (first term) goal)
     (let ((table (make-hash-table :test #'eq)))
       (make-clause (%freshen-term term table))))
    (t
     (%invalid-goal goal "a dynamic clause must be a fact or (:- HEAD BODY...)"))))

(defun %entry-predicate-arity (entry)
  (let ((head (%entry-head entry)))
    (values (first head) (length (rest head)))))

(define-condition arithmetic-evaluation-error (error)
  ((expression :initarg :expression :reader arithmetic-error-expression)
   (reason :initarg :reason :reader arithmetic-error-reason))
  (:report (lambda (condition stream)
             (format stream "Cannot evaluate Prolog arithmetic expression ~S: ~A."
                     (arithmetic-error-expression condition)
                     (arithmetic-error-reason condition))))
  (:documentation "Signalled when a Prolog arithmetic expression cannot be evaluated."))

(defun %arithmetic-error (expression reason &rest arguments)
  (error 'arithmetic-evaluation-error
         :expression expression
         :reason (apply #'format nil reason arguments)))

(defun %check-arithmetic-arity (expression arguments expected)
  (unless (= (length arguments) expected)
    (%arithmetic-error expression "operator ~S expects ~D argument~:P, got ~D"
                       (first expression) expected (length arguments))))

(defun %evaluate-arithmetic-expression (expression environment)
  "Evaluate a ground arithmetic EXPRESSION after applying ENVIRONMENT."
  (let ((resolved (logic-substitute expression environment)))
    (labels ((evaluate (term)
               (cond
                 ((numberp term) term)
                 ((logic-var-p term)
                  (%arithmetic-error expression "variable ~S is unbound" term))
                 ((not (consp term))
                  (%arithmetic-error expression "~S is not a number or arithmetic expression" term))
                 ((not (%proper-list-p term))
                  (%arithmetic-error expression "~S is not a proper arithmetic expression" term))
                 (t
                  (let ((operator (first term))
                        (arguments (rest term)))
                    (case operator
                      ((+ * / mod)
                       (%check-arithmetic-arity term arguments 2)
                       (let ((left (evaluate (first arguments)))
                             (right (evaluate (second arguments))))
                         (case operator
                           (+ (+ left right))
                           (* (* left right))
                           (/ (when (zerop right)
                                (%arithmetic-error expression "division by zero in ~S" term))
                              (/ left right))
                           (mod (unless (and (integerp left) (integerp right))
                                  (%arithmetic-error expression
                                                     "MOD operands must be integers in ~S" term))
                                (when (zerop right)
                                  (%arithmetic-error expression "division by zero in ~S" term))
                                (mod left right)))))
                      (-
                       (unless (member (length arguments) '(1 2))
                         (%arithmetic-error term "operator - expects one or two arguments, got ~D"
                                            (length arguments)))
                       (if (rest arguments)
                           (- (evaluate (first arguments))
                              (evaluate (second arguments)))
                           (- (evaluate (first arguments)))))
                      (abs
                       (%check-arithmetic-arity term arguments 1)
                       (abs (evaluate (first arguments))))
                      (otherwise
                       (%arithmetic-error expression "unknown arithmetic operator ~S" operator))))))))
      (evaluate resolved))))

;;; Control builtins

(define-builtin (true) (rulebase environment depth emit)
  (funcall emit environment))

(define-builtin (!) (rulebase environment depth emit)
  (funcall emit environment)
  (%propagate-cut))

(define-builtin (= left right) (rulebase environment depth emit)
  (%unify-emit left right environment emit))

(define-builtin ((/= !=) left right) (rulebase environment depth emit)
  (unless (nth-value 1 (unify left right environment))
    (funcall emit environment)))

(define-builtin (not goal) (rulebase environment depth emit)
  (unless (%provable-p goal rulebase environment (1- depth))
    (funcall emit environment)))

(define-builtin (call goal) (rulebase environment depth emit)
  (%prove-goal-sequence
   (%normalize-query (logic-substitute goal environment))
   rulebase environment (1- depth) emit))

(define-builtin (once goal) (rulebase environment depth emit)
  (block first-proof
    (%prove-goal-sequence
     (%normalize-query (logic-substitute goal environment))
     rulebase environment (1- depth)
     (lambda (extended)
       (funcall emit extended)
       (return-from first-proof nil)))))

(define-builtin (repeat) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (loop (funcall emit environment)))

(define-builtin (and &rest goals) (rulebase environment depth emit)
  (when (%prove-goal-sequence goals rulebase environment depth emit)
    (%propagate-cut)))

(define-builtin (or &rest alternatives) (rulebase environment depth emit)
  (dolist (alternative alternatives)
    (when (%prove-goal-sequence (%normalize-query alternative)
                                rulebase environment depth emit)
      (%propagate-cut))))

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
    (%prove-goal-sequence
     (%normalize-query (logic-substitute goal environment))
     rulebase environment (1- depth)
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
    (%prove-goal-sequence
     (%normalize-query (logic-substitute goal environment))
     rulebase environment (1- depth)
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

(defun %prolog-term-rank (term)
  (cond
    ((logic-var-p term) 0)
    ((numberp term) 1)
    ((symbolp term) 2)
    ((consp term) 3)
    (t 4)))

(defun %symbol-order-name (symbol)
  (format nil "~A::~A"
          (if (symbol-package symbol) (package-name (symbol-package symbol)) "")
          (symbol-name symbol)))

(defun %prolog-term< (left right)
  "A deterministic approximation of the standard Prolog term order."
  (let ((left-rank (%prolog-term-rank left))
        (right-rank (%prolog-term-rank right)))
    (cond
      ((/= left-rank right-rank) (< left-rank right-rank))
      ((logic-var-p left) (string< (symbol-name left) (symbol-name right)))
      ((numberp left) (< left right))
      ((symbolp left) (string< (%symbol-order-name left) (%symbol-order-name right)))
      ((consp left)
       (if (equal (car left) (car right))
           (%prolog-term< (cdr left) (cdr right))
           (%prolog-term< (car left) (car right))))
      (t (string< (prin1-to-string left) (prin1-to-string right))))))

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

;;; Dynamic database builtins

(define-builtin (asserta clause) (rulebase environment depth emit)
  (let* ((goal (list 'asserta clause))
         (entry (%clause-term-entry (logic-substitute clause environment) goal)))
    (rulebase-insert-clause! rulebase entry :position :first)
    (funcall emit environment)))

(define-builtin (assertz clause) (rulebase environment depth emit)
  (let* ((goal (list 'assertz clause))
         (entry (%clause-term-entry (logic-substitute clause environment) goal)))
    (rulebase-insert-clause! rulebase entry :position :last)
    (funcall emit environment)))

(define-builtin (retract clause) (rulebase environment depth emit)
  (let* ((goal (list 'retract clause))
         (pattern (logic-substitute clause environment)))
    (when (and (consp pattern)
               (not (eq (first pattern) ':-)))
      (%ensure-dynamic-predicate (first pattern) goal))
    (when (and (consp pattern) (eq (first pattern) ':-)
               (consp (rest pattern)) (consp (second pattern)))
      (%ensure-dynamic-predicate (first (second pattern)) goal))
    (dolist (entry (copy-list (rulebase-clauses rulebase)))
      (let* ((fresh (%freshen-dynamic-clause entry))
            (stored (if (null (clause-body fresh))
                         (%entry-head fresh)
                         (list* ':- (%entry-head fresh)
                                (clause-body fresh)))))
        (multiple-value-bind (extended ok)
            (unify clause stored environment)
          (when ok
            (rulebase-remove-clause! rulebase entry)
            (funcall emit extended)))))))

(define-builtin (abolish indicator) (rulebase environment depth emit)
  (let* ((goal (list 'abolish indicator))
         (resolved (logic-substitute indicator environment)))
    (unless (and (%proper-list-p resolved)
                 (= (length resolved) 3)
                 (symbolp (first resolved))
                 (eq (second resolved) '/)
                 (typep (third resolved) '(integer 0)))
      (%invalid-goal goal "predicate indicator must have shape (NAME / ARITY)"))
    (destructuring-bind (predicate slash arity) resolved
      (declare (ignore slash))
      (%ensure-dynamic-predicate predicate goal)
      (dolist (entry (copy-list (rulebase-clauses rulebase)))
        (multiple-value-bind (entry-predicate entry-arity)
            (%entry-predicate-arity entry)
          (when (and (eq predicate entry-predicate) (= arity entry-arity))
            (rulebase-remove-clause! rulebase entry)))))
    (funcall emit environment)))

(define-builtin (clause head body) (rulebase environment depth emit)
  (let ((resolved-head (logic-substitute head environment)))
    (when (consp resolved-head)
      (%ensure-dynamic-predicate (first resolved-head) (list 'clause head body)))
    (dolist (entry (copy-list (rulebase-clauses rulebase)))
      (let ((fresh (%freshen-dynamic-clause entry)))
        (multiple-value-bind (head-environment head-ok)
            (unify head (%entry-head fresh) environment)
          (when head-ok
            (%unify-emit body (%entry-body-term fresh) head-environment emit)))))))

;;; Arithmetic builtins

(define-builtin (is result expression) (rulebase environment depth emit)
  (%unify-emit result
               (%evaluate-arithmetic-expression expression environment)
               environment emit))

(define-builtin (|=:=| left right) (rulebase environment depth emit)
  (when (= (%evaluate-arithmetic-expression left environment)
           (%evaluate-arithmetic-expression right environment))
    (funcall emit environment)))

(define-builtin (|=\=| left right) (rulebase environment depth emit)
  (unless (= (%evaluate-arithmetic-expression left environment)
             (%evaluate-arithmetic-expression right environment))
    (funcall emit environment)))

(define-builtin (< left right) (rulebase environment depth emit)
  (when (< (%evaluate-arithmetic-expression left environment)
           (%evaluate-arithmetic-expression right environment))
    (funcall emit environment)))

(define-builtin (=< left right) (rulebase environment depth emit)
  (when (<= (%evaluate-arithmetic-expression left environment)
            (%evaluate-arithmetic-expression right environment))
    (funcall emit environment)))

(define-builtin (> left right) (rulebase environment depth emit)
  (when (> (%evaluate-arithmetic-expression left environment)
           (%evaluate-arithmetic-expression right environment))
    (funcall emit environment)))

(define-builtin (>= left right) (rulebase environment depth emit)
  (when (>= (%evaluate-arithmetic-expression left environment)
            (%evaluate-arithmetic-expression right environment))
    (funcall emit environment)))

;;; List builtins

(define-builtin (member item list-term) (rulebase environment depth emit)
  (let ((elements (logic-substitute list-term environment)))
    (when (%proper-list-p elements)
      (dolist (element elements)
        (%unify-emit item element environment emit)))))

(define-builtin (append left right result) (rulebase environment depth emit)
  (let ((left-value (logic-substitute left environment))
        (result-value (logic-substitute result environment)))
    (cond
      ((%proper-list-p left-value)
       (%unify-emit result
                    (append left-value (logic-substitute right environment))
                    environment emit))
      ((%proper-list-p result-value)
       (loop for split from 0 to (length result-value)
             do (multiple-value-bind (extended ok)
                    (unify left (subseq result-value 0 split) environment)
                  (when ok
                    (%unify-emit right (nthcdr split result-value)
                                 extended emit))))))))

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
