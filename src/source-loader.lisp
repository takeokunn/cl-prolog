;;;; Transactional Prolog source loading and directive evaluation.

(in-package #:cl-prolog)

(defun %call-with-prolog-source-stream (source function)
  (etypecase source
    (string
     (with-input-from-string (stream source)
       (funcall function stream)))
    (stream (funcall function source))
    (pathname
     (with-open-file (stream source :direction :input)
       (funcall function stream)))))

(defun %read-source-term (stream operator-table)
  (let* ((source (%read-prolog-term-source stream))
         (parser (%parser (%tokenize-prolog source operator-table) operator-table)))
    (if (eq :eof (%token-kind (%current-token parser)))
        (values nil nil)
        (let ((term (%parse-expression parser (make-hash-table :test #'equal) 0)))
          (%expect-token parser :operator ".")
          (%expect-token parser :eof)
          (values term t)))))

(defun %predicate-indicator-values (indicator directive)
  (let ((key (%predicate-indicator-key indicator directive)))
    (values (car key) (cdr key))))

(defun %operator-specifier-keyword (specifier)
  (unless (symbolp specifier)
    (error "Operator specifier must be an atom, got ~S." specifier))
  (let ((keyword (intern (symbol-name specifier) '#:keyword)))
    (unless (%valid-operator-specifier-p keyword)
      (error "Invalid operator specifier ~S." specifier))
    keyword))

(defun %apply-source-directive! (goal rulebase initializations module)
  (unless (consp goal)
    (error "Unknown Prolog directive ~S." goal))
  (case (first goal)
    (op
     (unless (= (length goal) 4)
       (error "OP directive requires priority, specifier, and name."))
     (destructuring-bind (name priority specifier operator) goal
       (declare (ignore name))
       (setf (rulebase-operator-table rulebase)
             (%operator-table-define
              (rulebase-operator-table rulebase)
              operator priority (%operator-specifier-keyword specifier)))))
    (dynamic
     (unless (= (length goal) 2)
       (error "DYNAMIC directive requires one predicate indicator."))
     (multiple-value-bind (predicate arity)
         (%predicate-indicator-values (second goal) 'dynamic)
       (%set-rulebase-predicate-property! rulebase predicate arity :dynamic module)))
    (use_module
     (unless (member (length goal) '(2 3))
       (error "USE_MODULE directive requires a module and optional imports."))
     (module-registry-import! (rulebase-module-registry rulebase)
                              module (second goal) (third goal)))
    (initialization
     (unless (= (length goal) 2)
       (error "INITIALIZATION directive requires one callable goal."))
     (push (cons module (second goal)) (car initializations)))
    (otherwise
     (error "Unknown Prolog directive ~S." goal))))

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
        (%set-rulebase-predicate-property! rulebase predicate arity :static module)))
    (rulebase-insert-clause! rulebase clause :module module)))

(defun %load-prolog-source-transaction (stream rulebase)
  (let ((initializations (list '()))
        (module +default-prolog-module+)
        (module-declared-p nil)
        (content-seen-p nil))
    (loop
      (multiple-value-bind (term present-p)
          (%read-source-term stream (rulebase-operator-table rulebase))
        (unless present-p (return))
        (cond
          ((and (consp term) (eq (first term) '?-))
           (error "Queries are not consultable source forms: ~S." term))
          ((and (consp term)
                (eq (first term) (%prolog-symbol ":-"))
                (= (length term) 2))
           (let ((directive (second term)))
             (if (and (consp directive) (eq (first directive) 'module))
                 (progn
                   (when (or content-seen-p module-declared-p)
                     (error "MODULE directive must be the unique first source term."))
                   (unless (= (length directive) 3)
                     (error "MODULE directive requires a name and export list."))
                   (setf module (second directive)
                         module-declared-p t)
                   (module-registry-declare!
                    (rulebase-module-registry rulebase)
                    module (third directive)))
                 (progn
                   (setf content-seen-p t)
                   (%apply-source-directive!
                    directive rulebase initializations module)))))
          (t
           (setf content-seen-p t)
           (%insert-source-clause! rulebase term module)))))
    (when module-declared-p
      (module-registry-validate-exports
       (rulebase-module-registry rulebase) module
       (lambda (candidate predicate arity)
         (not (null (%rulebase-predicate-property
                     rulebase predicate arity candidate))))))
    (dolist (initialization (nreverse (car initializations)))
      (unless (prolog-succeeds-p
               rulebase
               (list (%prolog-symbol ":")
                     (car initialization) (cdr initialization)))
        (error "Prolog initialization failed: ~S." (cdr initialization))))
    rulebase))

(defun consult-prolog (source &optional (rulebase (make-rulebase)))
  "Consult SOURCE and atomically replace RULEBASE after successful validation."
  (let ((transaction (%copy-rulebase rulebase)))
    (%call-with-prolog-source-stream
     source
     (lambda (stream)
       (%load-prolog-source-transaction stream transaction)))
    (%replace-rulebase! rulebase transaction)))
