(in-package #:cl-prolog)

(defun %remove-source-clauses! (rulebase canonical)
  (multiple-value-bind (revision entries) (%rulebase-snapshot rulebase)
    (declare (ignore revision))
    (%rulebase-retract-entries!
     rulebase
     (remove canonical entries :test-not #'equal
                               :key #'%stored-clause-source))))

(defun %remove-source-operators! (rulebase record)
  ;; Effects are pushed during loading, so replaying them restores prior layers.
  (dolist (effect (%source-record-operators record))
    (destructuring-bind (name specifier previous-priority source-priority) effect
      (let ((current (%operator-table-find
                      (rulebase-operator-table rulebase) name specifier)))
        (when (if (zerop source-priority)
                  (null current)
                  (and current
                       (= source-priority
                          (operator-definition-priority (first current)))))
          (setf (rulebase-operator-table rulebase)
                (%operator-table-define
                 (rulebase-operator-table rulebase) name
                 (or previous-priority 0) specifier)))))))

(defun %source-operator-overrides (rulebase record)
  "Return runtime definitions currently shadowing RECORD's latest effects."
  (let ((seen '())
        (overrides '()))
    (dolist (effect (%source-record-operators record) overrides)
      (destructuring-bind (name specifier previous-priority source-priority) effect
        (declare (ignore previous-priority))
        (let ((key (list name specifier)))
          (unless (member key seen :test #'equal)
            (push key seen)
            (let ((current (%operator-table-find
                            (rulebase-operator-table rulebase) name specifier)))
              (when (and current
                         (/= source-priority
                             (operator-definition-priority (first current))))
                (push (first current) overrides)))))))))

(defun %restore-operator-overrides! (rulebase overrides)
  (dolist (definition overrides)
    (setf (rulebase-operator-table rulebase)
          (%operator-table-define
           (rulebase-operator-table rulebase)
           (operator-definition-name definition)
           (operator-definition-priority definition)
           (operator-definition-specifier definition)))))

(defun %remove-source-predicate-properties! (rulebase record)
  (dolist (effect (%source-record-predicate-properties record))
    (destructuring-bind (module predicate arity property) effect
      (when (eq property
                (%rulebase-predicate-property
                 rulebase predicate arity module))
        (%remove-rulebase-predicate-property!
         rulebase predicate arity module)))))

(defun %remove-source-table-declarations! (rulebase record)
  (dolist (effect (%source-record-table-declarations record))
    (destructuring-bind (module predicate arity owner) effect
      (%remove-rulebase-table-declaration!
       rulebase predicate arity owner module))))

(defun %remove-source-artifacts! (rulebase canonical record)
  (%remove-source-clauses! rulebase canonical)
  (%remove-source-operators! rulebase record)
  (%remove-source-predicate-properties! rulebase record)
  (%remove-source-table-declarations! rulebase record))

(defun %load-prolog-source-transaction (stream rulebase initializations)
  (let ((module +default-prolog-module+)
        (module-declared-p nil)
        (content-seen-p nil))
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
    rulebase))

(defun %run-source-initializations! (initializations rulebase)
  (dolist (initialization (nreverse (car initializations)))
      (unless (prolog-succeeds-p
               rulebase
               (list (%prolog-symbol ":")
                     (car initialization) (cdr initialization)))
        (error "Prolog initialization failed: ~S." (cdr initialization))))
  rulebase)
