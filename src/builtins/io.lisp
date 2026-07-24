;;;; ISO-style stream builtins over the rulebase-owned I/O context.

(in-package #:cl-prolog)

(defun %io-operation (name)
  (%iso-atom name))

(defun %io-resolve-term (term environment operation)
  (let ((value (logic-substitute term environment)))
    (when (logic-var-p value)
      (%raise-instantiation-error environment operation
                                  "I/O argument must be instantiated"))
    value))

(defun %io-option-name-p (term name)
  (and (symbolp term) (string-equal (symbol-name term) name)))

(defun %io-boolean (value environment operation)
  (cond
    ((%io-option-name-p value "true") t)
    ((%io-option-name-p value "false") nil)
    ((logic-var-p value)
     (%raise-instantiation-error environment operation
                                 "Option value must be instantiated"))
    (t (%raise-domain-error "STREAM_OPTION" value environment operation
                            "Expected true or false"))))

(defparameter +io-read-option-names+
  '("variables" "variable_names" "singletons" "syntax_errors"))

(defun %io-parse-option-list (term environment operation domain-name allowed
                              instantiation-predicate instantiation-message
                              list-message option-message unsupported-message)
  (let ((options (logic-substitute term environment))
        (result '()))
    (when (funcall instantiation-predicate options)
      (%raise-instantiation-error environment operation instantiation-message))
    (unless (%proper-list-p options)
      (%raise-type-error "LIST" options environment operation list-message))
    (dolist (option options (nreverse result))
      (unless (and (%proper-list-p option) (= (length option) 2)
                   (symbolp (first option)))
        (%raise-domain-error domain-name option environment operation option-message))
      (let ((name (find (symbol-name (first option)) allowed
                        :test #'string-equal)))
        (unless name
          (%raise-domain-error domain-name option environment operation
                               unsupported-message))
        (push (cons name (second option)) result)))))

(defun %io-options (term environment operation allowed)
  (%io-parse-option-list
   term environment operation "STREAM_OPTION" allowed
   #'%term-has-variables-p
   "Stream options must be instantiated"
   "Stream options must be a proper list"
   "Expected a unary stream option"
   "Unsupported stream option"))

(defun %io-option (name options &optional default)
  (let ((entry (assoc name options :test #'string-equal)))
    (if entry (cdr entry) default)))

(defun %io-read-options (term environment operation)
  (%io-parse-option-list
   term environment operation "READ_OPTION" +io-read-option-names+
   ;; READ options may carry output variables in option values
   ;; (e.g. variables/2 and variable_names/2), so only reject the
   ;; options list itself when it is still a logic variable.
   #'logic-var-p
   "Read options must be instantiated"
   "Read options must be a proper list"
   "Expected a unary read option"
   "Unsupported read option"))

(defun %io-syntax-errors-mode (options environment operation)
  (let ((mode (logic-substitute
               (%io-option "syntax_errors" options (%iso-atom "error"))
               environment)))
    (when (logic-var-p mode)
      (%raise-instantiation-error environment operation
                                  "syntax_errors must be instantiated"))
    (unless (and (symbolp mode)
                 (member (symbol-name mode) '("ERROR" "FAIL" "QUIET")
                         :test #'string-equal))
      (%raise-domain-error "READ_OPTION" mode environment operation
                           "syntax_errors must be error, fail, or quiet"))
    mode))

(defun %io-pathname (term environment operation)
  (let ((value (%io-resolve-term term environment operation)))
    (unless (symbolp value)
      (%raise-type-error "ATOM" value environment operation
                         "Stream source must be an atom"))
    (let ((name (symbol-name value)))
      ;; Unquoted atoms are interned uppercase; quoted mixed-case atoms retain
      ;; their spelling and must remain usable on case-sensitive filesystems.
      (if (string= name (string-upcase name))
          (string-downcase name)
          name))))

(defun %io-public-designator (entry)
  (or (prolog-stream-alias entry) (prolog-stream-handle entry)))

(defun %io-context (rulebase)
  (rulebase-io-context rulebase))

(defun %io-current-input-entry (rulebase)
  (prolog-io-context-current-input (%io-context rulebase)))

(defun %io-current-output-entry (rulebase)
  (prolog-io-context-current-output (%io-context rulebase)))

(defmacro %define-io-dual-builtin ((name current-arguments explicit-arguments operation)
                                   (rulebase environment depth emit)
                                   &body clauses)
  ;; CLAUSES is a flat (:current FORM :explicit FORM) plist; a NIL lookup
  ;; would silently compile a builtin whose body always fails, so reject
  ;; malformed clauses at macroexpansion time.
  (unless (and (= (length clauses) 4)
               (eq (first clauses) :current)
               (eq (third clauses) :explicit))
    (error "~S expects (:CURRENT form :EXPLICIT form), got ~S for ~S"
           '%define-io-dual-builtin clauses name))
  (let ((current-form (getf clauses :current))
        (explicit-form (getf clauses :explicit)))
    `(progn
       (define-builtin (,name ,@current-arguments) (,rulebase ,environment ,depth ,emit)
         (declare (ignore ,depth))
         (let ((operation (%io-operation ,operation)))
           ,current-form))
       (define-builtin (,name stream ,@explicit-arguments)
           (,rulebase ,environment ,depth ,emit)
         (declare (ignore ,depth))
         (let ((operation (%io-operation ,operation)))
           ,explicit-form)))))

(defun %io-stream-entry (rulebase term direction environment operation)
  (%resolve-prolog-stream (%io-context rulebase)
                          (%io-resolve-term term environment operation)
                          direction environment operation))

(defun %io-character (term environment operation)
  (let ((value (%io-resolve-term term environment operation)))
    (unless (and (symbolp value) (= (length (symbol-name value)) 1))
      (%raise-type-error "CHARACTER" value environment operation
                         "Expected a one-character atom"))
    (char (symbol-name value) 0)))

(defun %io-register-open-stream (rulebase source mode options environment operation)
  (let* ((type (%io-option "type" options (%iso-atom "text")))
         (alias (%io-option "alias" options))
         (path (%io-pathname source environment operation)))
    (unless (and (symbolp type)
                 (or (%io-option-name-p type "text")
                     (%io-option-name-p type "binary")))
      (%raise-domain-error "STREAM_OPTION" type environment operation
                           "Stream type must be text or binary"))
    (%check-stream-alias alias environment operation)
    (multiple-value-bind (access-mode direction)
        (%validate-prolog-stream-mode mode environment operation)
      (let ((stream (handler-case
                        (open path
                              :direction (if (eq direction :input) :input :output)
                              :element-type (if (%io-option-name-p type "binary")
                                                '(unsigned-byte 8)
                                                'character)
                              :if-does-not-exist (if (eq direction :input) nil :create)
                              :if-exists (case access-mode
                                           ((:append) :append)
                                           (otherwise :supersede)))
                      (file-error () nil))))
        (unless stream
          (%raise-existence-error "SOURCE_SINK" source environment operation
                                  "Cannot open source or sink"))
        (handler-case
            (%register-prolog-stream! (%io-context rulebase) stream access-mode
                                      :alias alias :source path
                                      :type (if (%io-option-name-p type "binary")
                                                :binary
                                                :text)
                                      :environment environment :operation operation)
          (error (condition)
            (close stream :abort t)
            (error condition)))))))

(defun %io-open-goal (rulebase source mode stream options environment emit)
  (let* ((operation (%io-operation "OPEN"))
         (mode-value (%io-resolve-term mode environment operation))
         (parsed-options (%io-options options environment operation '("alias" "type")))
         (entry (%io-register-open-stream rulebase source mode-value parsed-options
                                          environment operation)))
    (multiple-value-bind (extended ok)
        (unify stream (%io-public-designator entry) environment)
      (if ok
          (funcall emit extended)
          (%close-prolog-stream! (%io-context rulebase) entry environment operation)))))

(define-builtin (open source mode stream) (rulebase environment depth emit)
  (declare (ignore depth))
  (%io-open-goal rulebase source mode stream '() environment emit))

(define-builtin (open source mode stream options) (rulebase environment depth emit)
  (declare (ignore depth))
  (%io-open-goal rulebase source mode stream options environment emit))

(defun %io-close-goal (rulebase stream options environment emit)
  (let* ((operation (%io-operation "CLOSE"))
         (parsed (%io-options options environment operation '("force")))
         (force (%io-option "force" parsed (%iso-atom "false"))))
    (%io-boolean force environment operation)
    (%close-prolog-stream! (%io-context rulebase)
                           (%io-resolve-term stream environment operation)
                           environment operation)
    (funcall emit environment)))

(define-builtin (close stream) (rulebase environment depth emit)
  (declare (ignore depth))
  (%io-close-goal rulebase stream '() environment emit))

(define-builtin (close stream options) (rulebase environment depth emit)
  (declare (ignore depth))
  (%io-close-goal rulebase stream options environment emit))

(defun %io-stream-entries (rulebase)
  (let ((entries '()))
    (maphash (lambda (handle entry)
               (declare (ignore handle))
               (when (open-stream-p (prolog-stream-stream entry))
                 (push entry entries)))
             (prolog-io-context-streams (%io-context rulebase)))
    (sort entries #'string<
          :key (lambda (entry)
                 (symbol-name (%io-public-designator entry))))))

(defun %io-stream-position (entry)
  (ignore-errors (file-position (prolog-stream-stream entry))))

(defun %io-end-of-stream-state (entry environment operation)
  (when (eq (prolog-stream-direction entry) :input)
    (%io-at-end-p entry environment operation))
  (%iso-atom
   (ecase (prolog-stream-end-of-stream entry)
     (:not "not")
     (:at "at")
     (:past "past"))))

(defun %io-stream-properties (entry environment operation)
  (let ((position (%io-stream-position entry)))
    (append
     (when (or (stringp (prolog-stream-source entry))
               (pathnamep (prolog-stream-source entry)))
       (list (list (%iso-atom "file_name")
                   (%prolog-atom-symbol
                    (princ-to-string (prolog-stream-source entry))
                    :preserve-case t))))
     (list (list (%iso-atom "mode")
                 (%iso-atom (string-downcase
                             (symbol-name (prolog-stream-mode entry))))))
     (when (prolog-stream-alias entry)
       (list (list (%iso-atom "alias") (prolog-stream-alias entry))))
     (when (integerp position)
       (list (list (%iso-atom "position") position)))
     (list (list (%iso-atom "end_of_stream")
                 (%io-end-of-stream-state entry environment operation))
           (list (%iso-atom "eof_action") (%iso-atom "eof_code"))
           (list (%iso-atom "reposition")
                 (%iso-atom (if (integerp position) "true" "false")))
           (list (%iso-atom "type")
                 (%iso-atom (string-downcase
                             (symbol-name (prolog-stream-type entry)))))))))

(defun %io-stream-property-shape-p (property)
  (and (%proper-list-p property)
       (= (length property) 2)
       (symbolp (first property))
       (member (symbol-name (first property))
               '("FILE_NAME" "MODE" "ALIAS" "POSITION" "END_OF_STREAM"
                 "EOF_ACTION" "REPOSITION" "TYPE")
               :test #'string-equal)))

(defun %io-property-candidates (rulebase stream environment operation)
  (let ((designator (logic-substitute stream environment)))
    (if (logic-var-p designator)
        (%io-stream-entries rulebase)
        (list (%resolve-prolog-stream (%io-context rulebase) designator nil
                                      environment operation)))))

(define-builtin (stream_property stream property)
    (rulebase environment depth emit)
  (declare (ignore depth))
  (let* ((operation (%io-operation "STREAM_PROPERTY"))
         (resolved-property (logic-substitute property environment)))
    (unless (or (logic-var-p resolved-property)
                (%io-stream-property-shape-p resolved-property))
      (%raise-domain-error "STREAM_PROPERTY" resolved-property environment
                           operation "Invalid stream property"))
    (dolist (entry (%io-property-candidates rulebase stream environment operation))
      (multiple-value-bind (stream-environment stream-ok)
          (unify stream (%io-public-designator entry) environment)
        (when stream-ok
          (dolist (candidate (%io-stream-properties entry environment operation))
            (multiple-value-bind (extended ok)
                (unify property candidate stream-environment)
              (when ok (funcall emit extended)))))))))

(define-builtin (set_stream_position stream position)
    (rulebase environment depth emit)
  (declare (ignore depth))
  (let* ((operation (%io-operation "SET_STREAM_POSITION"))
         (entry (%io-stream-entry rulebase stream nil environment operation))
         (resolved-position (%io-resolve-term position environment operation)))
    (unless (integerp resolved-position)
      (%raise-type-error "INTEGER" resolved-position environment operation
                         "Stream position must be an integer"))
    (when (minusp resolved-position)
      (%raise-domain-error "NOT_LESS_THAN_ZERO" resolved-position environment
                           operation "Stream position must not be negative"))
    (unless (ignore-errors
              (file-position (prolog-stream-stream entry) resolved-position))
      (%raise-permission-error "REPOSITION" "STREAM"
                               (%io-public-designator entry)
                               environment operation
                               "Stream does not support repositioning"))
    (setf (prolog-stream-end-of-stream entry) :not)
    (funcall emit environment)))

(defmacro %define-current-stream-builtin (&body definitions)
  "Each of DEFINITIONS is (NAME ACCESSOR); ACCESSOR retrieves the rulebase's
current input or output stream entry, which NAME unifies with its public
designator."
  `(progn
     ,@(loop for (name accessor) in definitions
             collect `(define-builtin (,name stream) (rulebase environment depth emit)
                        (declare (ignore depth))
                        (%unify-emit stream
                                     (%io-public-designator (,accessor rulebase))
                                     environment emit)))))

(%define-current-stream-builtin
  (current_input %io-current-input-entry)
  (current_output %io-current-output-entry))

(defmacro %define-set-stream-builtin (&body definitions)
  "Each of DEFINITIONS is (NAME DIRECTION CONTEXT-ACCESSOR OPERATION-NAME);
NAME resolves STREAM under DIRECTION and installs it as the rulebase's
current stream for that direction via (SETF CONTEXT-ACCESSOR)."
  `(progn
     ,@(loop for (name direction context-accessor operation-name) in definitions
             collect `(define-builtin (,name stream) (rulebase environment depth emit)
                        (declare (ignore depth))
                        (let ((operation (%io-operation ,operation-name)))
                          (setf (,context-accessor (%io-context rulebase))
                                (%io-stream-entry rulebase stream ,direction environment operation))
                          (funcall emit environment))))))

(%define-set-stream-builtin
  (set_input :input prolog-io-context-current-input "SET_INPUT")
  (set_output :output prolog-io-context-current-output "SET_OUTPUT"))

(defun %io-parse-term-with-variables (input operator-table)
  (let* ((parser (%parser (%tokenize-prolog input operator-table) operator-table))
         (variables (make-hash-table :test #'equal))
         (term (%parse-expression parser variables 0)))
    (%accept-token parser :operator ".")
    (%expect-token parser :eof)
    (let ((names (make-hash-table :test #'eq))
          (occurrences (make-hash-table :test #'eq)))
      (maphash (lambda (name variable) (setf (gethash variable names) name)) variables)
      (labels ((count-variables (node)
                 (cond
                   ((logic-var-p node) (incf (gethash node occurrences 0)))
                   ((consp node)
                    (count-variables (car node))
                    (count-variables (cdr node))))))
        (count-variables term))
      (let ((ordered (%collect-variables term)))
        (let ((named
                (loop for variable in ordered
                      for name = (gethash variable names)
                      when name
                        collect (list '= (%prolog-atom-symbol name :preserve-case t)
                                      variable))))
          (values term ordered named
                  (loop for entry in named
                        when (= 1 (gethash (third entry) occurrences 0))
                          collect entry)))))))
