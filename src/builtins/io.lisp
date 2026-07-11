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

(defun %io-options (term environment operation allowed)
  (let ((options (logic-substitute term environment))
        (result '()))
    (when (%term-has-variables-p options)
      (%raise-instantiation-error environment operation
                                  "Stream options must be instantiated"))
    (unless (%proper-list-p options)
      (%raise-type-error "LIST" options environment operation
                         "Stream options must be a proper list"))
    (dolist (option options (nreverse result))
      (unless (and (%proper-list-p option) (= (length option) 2)
                   (symbolp (first option)))
        (%raise-domain-error "STREAM_OPTION" option environment operation
                             "Expected a unary stream option"))
      (let ((name (find (symbol-name (first option)) allowed
                        :test #'string-equal)))
        (unless name
          (%raise-domain-error "STREAM_OPTION" option environment operation
                               "Unsupported stream option"))
        (push (cons name (second option)) result)))))

(defun %io-option (name options &optional default)
  (let ((entry (assoc name options :test #'string-equal)))
    (if entry (cdr entry) default)))

(defun %io-read-options (term environment operation)
  (let ((options (logic-substitute term environment))
        (result '()))
    (when (logic-var-p options)
      (%raise-instantiation-error environment operation
                                  "Read options must be instantiated"))
    (unless (%proper-list-p options)
      (%raise-type-error "LIST" options environment operation
                         "Read options must be a proper list"))
    (dolist (option options (nreverse result))
      (unless (and (%proper-list-p option) (= (length option) 2)
                   (symbolp (first option)))
        (%raise-domain-error "READ_OPTION" option environment operation
                             "Expected a unary read option"))
      (let ((name (find (symbol-name (first option))
                        '("variables" "variable_names" "syntax_errors")
                        :test #'string-equal)))
        (unless name
          (%raise-domain-error "READ_OPTION" option environment operation
                               "Unsupported read option"))
        (push (cons name (second option)) result)))))

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

(defun %io-character-atom (character)
  (%prolog-atom-symbol (string character) :preserve-case t))

(defun %io-register-open-stream (rulebase source mode options environment operation)
  (let* ((type (%io-option "type" options (%iso-atom "text")))
         (alias (%io-option "alias" options))
         (path (%io-pathname source environment operation)))
    (unless (%io-option-name-p type "text")
      (%raise-domain-error "STREAM_OPTION" type environment operation
                           "Only text streams are supported"))
    (when (and alias (not (symbolp alias)))
      (%raise-type-error "ATOM" alias environment operation
                         "Stream alias must be an atom"))
    (multiple-value-bind (access-mode direction)
        (%validate-prolog-stream-mode mode environment operation)
      (let ((stream (handler-case
                        (open path
                              :direction (if (eq direction :input) :input :output)
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
                                      :environment environment :operation operation)
          (error (condition)
            (close stream :abort t)
            (error condition)))))))

(define-builtin (open source mode stream options) (rulebase environment depth emit)
  (declare (ignore depth))
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

(define-builtin (close stream options) (rulebase environment depth emit)
  (declare (ignore depth))
  (let* ((operation (%io-operation "CLOSE"))
         (parsed (%io-options options environment operation '("force")))
         (force (%io-option "force" parsed (%iso-atom "false"))))
    (%io-boolean force environment operation)
    (%close-prolog-stream! (%io-context rulebase)
                           (%io-resolve-term stream environment operation)
                           environment operation)
    (funcall emit environment)))

(define-builtin (current_input stream) (rulebase environment depth emit)
  (declare (ignore depth))
  (%unify-emit stream
               (%io-public-designator
                (prolog-io-context-current-input (%io-context rulebase)))
               environment emit))

(define-builtin (current_output stream) (rulebase environment depth emit)
  (declare (ignore depth))
  (%unify-emit stream
               (%io-public-designator
                (prolog-io-context-current-output (%io-context rulebase)))
               environment emit))

(define-builtin (set_input stream) (rulebase environment depth emit)
  (declare (ignore depth))
  (let ((operation (%io-operation "SET_INPUT")))
    (setf (prolog-io-context-current-input (%io-context rulebase))
          (%io-stream-entry rulebase stream :input environment operation))
    (funcall emit environment)))

(define-builtin (set_output stream) (rulebase environment depth emit)
  (declare (ignore depth))
  (let ((operation (%io-operation "SET_OUTPUT")))
    (setf (prolog-io-context-current-output (%io-context rulebase))
          (%io-stream-entry rulebase stream :output environment operation))
    (funcall emit environment)))

(defun %io-parse-term-with-variables (input operator-table)
  (let* ((parser (%parser (%tokenize-prolog input operator-table) operator-table))
         (variables (make-hash-table :test #'equal))
         (term (%parse-expression parser variables 0)))
    (%accept-token parser :operator ".")
    (%expect-token parser :eof)
    (let ((names (make-hash-table :test #'eq)))
      (maphash (lambda (name variable) (setf (gethash variable names) name)) variables)
      (let ((ordered (%collect-variables term)))
        (values term ordered
                (mapcar (lambda (variable)
                          (list '= (%prolog-atom-symbol (gethash variable names)
                                                         :preserve-case t)
                                variable))
                        ordered))))))

(defun %io-read-term-values (entry operator-table mode environment operation)
  (let ((input (prolog-stream-stream entry)))
    (if (eq (peek-char t input nil :eof) :eof)
        (values (%iso-atom "end_of_file") '() '() t)
        (handler-case
            (multiple-value-bind (term variables names)
                (%io-parse-term-with-variables input operator-table)
              (values term variables names t))
          (error (condition)
            (if (%io-option-name-p mode "error")
                (%raise-domain-error "SYNTAX_ERROR" condition environment operation
                                     "Invalid Prolog term")
                (values nil nil nil nil)))))))

(defun %io-unify-read-results (term value options variables names environment emit)
  (multiple-value-bind (term-environment term-ok)
      (unify term value environment)
    (when term-ok
      (multiple-value-bind (variables-environment variables-ok)
          (unify (%io-option "variables" options variables)
                 variables term-environment)
        (when variables-ok
          (multiple-value-bind (names-environment names-ok)
              (unify (%io-option "variable_names" options names)
                     names variables-environment)
            (when names-ok (funcall emit names-environment))))))))

(defun %io-read-term-goal (rulebase entry term options environment emit)
  (let* ((operation (%io-operation "READ_TERM"))
         (parsed (%io-read-options options environment operation))
         (mode (%io-syntax-errors-mode parsed environment operation)))
    (multiple-value-bind (value variables names readablep)
        (%io-read-term-values entry (rulebase-operator-table rulebase) mode
                              environment operation)
      (when readablep
        (%io-unify-read-results term value parsed variables names environment emit)))))

(define-builtin (read_term term options) (rulebase environment depth emit)
  (declare (ignore depth))
  (%io-read-term-goal
   rulebase (prolog-io-context-current-input (%io-context rulebase))
   term options environment emit))

(define-builtin (read_term stream term options) (rulebase environment depth emit)
  (declare (ignore depth))
  (let ((operation (%io-operation "READ_TERM")))
    (%io-read-term-goal rulebase
                        (%io-stream-entry rulebase stream :input environment operation)
                        term options environment emit)))

(defun %io-write-term-goal (entry term options environment emit)
  (let* ((operation (%io-operation "WRITE_TERM"))
         (parsed (%io-options options environment operation
                              '("quoted" "ignore_ops" "numbervars")))
         (ignore-ops (%io-boolean
                      (%io-option "ignore_ops" parsed (%iso-atom "false"))
                      environment operation))
         (quoted (%io-boolean (%io-option "quoted" parsed (%iso-atom "false"))
                              environment operation))
         (numbervars
           (%io-boolean (%io-option "numbervars" parsed (%iso-atom "false"))
                        environment operation)))
    (let ((value (logic-substitute term environment))
          (stream (prolog-stream-stream entry)))
      (%write-prolog-term-with-options value stream
                                       :quoted quoted
                                       :numbervars numbervars
                                       :ignore-ops ignore-ops))
    (funcall emit environment)))

(define-builtin (write_term term options) (rulebase environment depth emit)
  (declare (ignore depth))
  (%io-write-term-goal
   (prolog-io-context-current-output (%io-context rulebase))
   term options environment emit))

(define-builtin (write_term stream term options) (rulebase environment depth emit)
  (declare (ignore depth))
  (let ((operation (%io-operation "WRITE_TERM")))
    (%io-write-term-goal
     (%io-stream-entry rulebase stream :output environment operation)
     term options environment emit)))

(defun %io-newline (rulebase entry)
  (terpri (prolog-stream-stream
           (or entry (prolog-io-context-current-output (%io-context rulebase))))))

(defun %io-newline-goal (rulebase stream environment emit)
  (%io-newline rulebase stream)
  (funcall emit environment))

(define-builtin (nl) (rulebase environment depth emit)
  (declare (ignore depth))
  (%io-newline-goal rulebase nil environment emit))

(define-builtin (nl stream) (rulebase environment depth emit)
  (declare (ignore depth))
  (%io-newline-goal
   rulebase
   (%io-stream-entry rulebase stream :output environment (%io-operation "NL"))
   environment emit))

(defun %io-flush (rulebase entry)
  (finish-output
   (prolog-stream-stream
    (or entry (prolog-io-context-current-output (%io-context rulebase))))))

(defun %io-flush-goal (rulebase stream environment emit)
  (%io-flush rulebase stream)
  (funcall emit environment))

(define-builtin (flush_output) (rulebase environment depth emit)
  (declare (ignore depth))
  (%io-flush-goal rulebase nil environment emit))

(define-builtin (flush_output stream) (rulebase environment depth emit)
  (declare (ignore depth))
  (%io-flush-goal
   rulebase
   (%io-stream-entry rulebase stream :output environment
                     (%io-operation "FLUSH_OUTPUT"))
   environment emit))

(defun %io-read-character (entry)
  (let ((character (read-char (prolog-stream-stream entry) nil nil)))
    (if character (%io-character-atom character) (%iso-atom "end_of_file"))))

(define-builtin (get_char character) (rulebase environment depth emit)
  (declare (ignore depth))
  (%unify-emit character
               (%io-read-character
                (prolog-io-context-current-input (%io-context rulebase)))
               environment emit))

(define-builtin (get_char stream character) (rulebase environment depth emit)
  (declare (ignore depth))
  (%unify-emit character
               (%io-read-character
                (%io-stream-entry rulebase stream :input environment
                                  (%io-operation "GET_CHAR")))
               environment emit))

(defun %io-write-character (entry term environment operation)
  (write-char (%io-character term environment operation)
              (prolog-stream-stream entry)))

(define-builtin (put_char character) (rulebase environment depth emit)
  (declare (ignore depth))
  (let ((operation (%io-operation "PUT_CHAR")))
    (%io-write-character
     (prolog-io-context-current-output (%io-context rulebase))
     character environment operation)
    (funcall emit environment)))

(define-builtin (put_char stream character) (rulebase environment depth emit)
  (declare (ignore depth))
  (let ((operation (%io-operation "PUT_CHAR")))
    (%io-write-character
     (%io-stream-entry rulebase stream :output environment operation)
     character environment operation)
    (funcall emit environment)))

(defun %io-at-end-p (entry)
  (eq (peek-char nil (prolog-stream-stream entry) nil :eof) :eof))

(define-builtin (at_end_of_stream) (rulebase environment depth emit)
  (declare (ignore depth))
  (when (%io-at-end-p
         (prolog-io-context-current-input (%io-context rulebase)))
    (funcall emit environment)))

(define-builtin (at_end_of_stream stream) (rulebase environment depth emit)
  (declare (ignore depth))
  (when (%io-at-end-p
         (%io-stream-entry rulebase stream :input environment
                           (%io-operation "AT_END_OF_STREAM")))
    (funcall emit environment)))
