;;;; Directive evaluation and clause insertion for one Prolog source
;;;; term at a time: op/dynamic/table/use_module/initialization/consult/
;;;; load_files/include directives, and recording their effects so
;;;; source-rollback.lisp can undo a reload.

(in-package #:cl-prolog)

(defun %prolog-query-term-p (term)
  "True when TERM is a bare query (?- Goal), which is not a consultable
source form."
  (and (consp term) (eq (first term) '?-)))

(defun %prolog-directive-term-p (term)
  "True when TERM is a 2-arity :- directive term (:- Goal), as opposed to a
3-arity clause term (:- Head Body)."
  (and (consp term)
       (eq (first term) (%prolog-symbol ":-"))
       (= (length term) 2)))

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
  (or (find (symbol-name specifier) +operator-specifiers+
            :key #'symbol-name
            :test #'string-equal)
      (error "Invalid operator specifier ~S." specifier)))

(defun %record-source-operator-effect! (effect)
  (when *current-prolog-source-record*
    (push effect (%source-record-operators *current-prolog-source-record*))))

(defmacro define-source-record-recorder (name accessor (&rest value-params))
  "Define NAME to push (MODULE PREDICATE ARITY . VALUE-PARAMS) onto the
active *CURRENT-PROLOG-SOURCE-RECORD*'s ACCESSOR place, a no-op outside a
source-loading transaction."
  `(defun ,name (module predicate arity ,@value-params)
     (when *current-prolog-source-record*
       (push (list module predicate arity ,@value-params)
             (,accessor *current-prolog-source-record*)))))

(define-source-record-recorder %record-source-predicate-property!
    %source-record-predicate-properties (property))

(define-source-record-recorder %record-source-table-declaration!
    %source-record-table-declarations (owner))

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
        ((%prolog-query-term-p term)
         (error "Queries are not consultable source forms: ~S." term))
        ((%prolog-directive-term-p term)
         (let ((directive (second term)))
           (when (and (consp directive) (eq (first directive) 'module))
             (error "MODULE directives are not allowed in included files."))
           (%apply-source-directive! directive rulebase initializations module)))
        (t
         (%insert-source-clause! rulebase term module))))))

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
