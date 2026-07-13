(in-package #:cl-prolog)

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
       (let* ((keyword (%operator-specifier-keyword specifier))
              (previous (%operator-table-find
                         (rulebase-operator-table rulebase) operator keyword)))
         (%record-source-operator-effect!
          (list operator keyword
                (and previous
                     (operator-definition-priority (first previous)))
                priority))
         (setf (rulebase-operator-table rulebase)
               (%operator-table-define
                (rulebase-operator-table rulebase)
                operator priority keyword)))))
    (dynamic
     (unless (= (length goal) 2)
       (error "DYNAMIC directive requires one predicate indicator."))
     (multiple-value-bind (predicate arity)
         (%predicate-indicator-values (second goal) 'dynamic)
       (%set-rulebase-predicate-property! rulebase predicate arity :dynamic module)
        (%record-source-predicate-property!
         module predicate arity :dynamic)))
    (table
     (unless (= (length goal) 2)
       (error "TABLE directive requires one predicate indicator."))
     (multiple-value-bind (predicate arity)
         (%predicate-indicator-values (second goal) 'table)
       (let ((owner (or *current-prolog-source* :anonymous-source)))
         (%add-rulebase-table-declaration!
          rulebase predicate arity owner module)
         (%record-source-table-declaration!
          module predicate arity owner))))
    (use_module
     (unless (member (length goal) '(2 3))
       (error "USE_MODULE directive requires a module and optional imports."))
     (module-registry-import! (rulebase-module-registry rulebase)
                              module (second goal) (third goal)))
    (initialization
     (unless (= (length goal) 2)
       (error "INITIALIZATION directive requires one callable goal."))
     (push (cons module (second goal)) (car initializations)))
    ((consult ensure_loaded)
     (unless (= (length goal) 2)
       (error "~A directive requires one source argument." (first goal)))
     (%load-prolog-source-into-rulebase
      (%source-file-pathnames (second goal) nil (first goal))
      rulebase initializations
      :if-loaded (if (eq (first goal) 'ensure_loaded) :skip :reload)))
    (load_files
     (unless (member (length goal) '(2 3))
       (error "LOAD_FILES directive requires sources and optional options."))
     (%load-prolog-source-into-rulebase
      (%source-file-pathnames (second goal) nil 'load_files)
      rulebase initializations
      :if-loaded (if (= (length goal) 2)
                     :reload
                     (%load-files-if-loaded-policy (third goal) nil 'load_files))))
    ((set_prolog_flag char_conversion)
     (unless (= (length goal) 3)
       (error "~A directive requires two arguments." (first goal)))
     (unless (prolog-succeeds-p rulebase goal)
       (error "Prolog directive failed: ~S." goal)))
    ((discontiguous multifile)
     (unless (rest goal)
       (error "~A directive requires predicate indicators." (first goal)))
     ;; The engine already resolves clauses independently of their textual
     ;; grouping and origin, so the declaration only needs validation.
     (dolist (indicator (rest goal))
       (%predicate-indicator-values indicator (first goal))))
    (include
     (unless (= (length goal) 2)
       (error "INCLUDE directive requires one source argument."))
     (dolist (pathname (%source-file-pathnames (second goal) nil 'include))
       (%call-with-prolog-source-stream
        (%canonical-prolog-source-pathname pathname)
        (lambda (stream)
          (%process-included-source-terms
           stream rulebase initializations module)))))
    (otherwise
     (error "Unknown Prolog directive ~S." goal))))

(defun %process-included-source-terms (stream rulebase initializations module)
  "Splice STREAM's terms into the including source unit under MODULE."
  (loop
    (multiple-value-bind (term present-p)
        (let ((*active-char-conversions*
                (%rulebase-active-char-conversions rulebase)))
          (%read-source-term stream (rulebase-operator-table rulebase)))
      (unless present-p (return))
      (cond
        ((and (consp term) (eq (first term) '?-))
         (error "Queries are not consultable source forms: ~S." term))
        ((and (consp term)
              (eq (first term) (%prolog-symbol ":-"))
              (= (length term) 2))
         (let ((directive (second term)))
           (when (and (consp directive) (eq (first directive) 'module))
             (error "MODULE directives are not allowed in included files."))
           (%apply-source-directive! directive rulebase initializations module)))
        (t
         (%insert-source-clause! rulebase term module))))))

