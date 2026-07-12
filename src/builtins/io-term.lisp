(in-package #:cl-prolog)

(%define-io-dual-builtin (write_canonical (term) (term) "WRITE_CANONICAL")
    (rulebase environment depth emit)
  :current
  (%io-write-canonical-goal
   (%io-current-output-entry rulebase)
   term environment emit operation)
  :explicit
  (%io-write-canonical-goal
   (%io-stream-entry rulebase stream :output environment operation)
   term environment emit operation))

(defun %io-write-canonical-goal (entry term environment emit operation)
  ;; ISO write_canonical/1,2: quoted, operator-free, no numbervars, so the
  ;; output reads back as the same term under any operator table.
  (%io-write-term-goal
   entry term
   (list (list (%iso-atom "quoted") (%iso-atom "true"))
         (list (%iso-atom "ignore_ops") (%iso-atom "true")))
   environment emit operation))

(%define-io-dual-builtin (writeq (term) (term) "WRITEQ")
    (rulebase environment depth emit)
  :current
  (%io-write-facade-goal
   (%io-current-output-entry rulebase)
   term t environment emit operation)
  :explicit
  (%io-write-facade-goal
   (%io-stream-entry rulebase stream :output environment operation)
   term t environment emit operation))

(%define-io-dual-builtin (write (term) (term) "WRITE")
    (rulebase environment depth emit)
  :current
  (%io-write-facade-goal
   (%io-current-output-entry rulebase)
   term nil environment emit operation)
  :explicit
  (%io-write-facade-goal
   (%io-stream-entry rulebase stream :output environment operation)
   term nil environment emit operation))

(defun %io-write-facade-goal (entry term quoted environment emit operation)
  (%io-write-term-goal
   entry term
   (list (list (%iso-atom "quoted") (%iso-atom (if quoted "true" "false")))
         (list (%iso-atom "numbervars") (%iso-atom "true")))
   environment emit operation))

(%define-io-dual-builtin (write_term (term options) (term options) "WRITE_TERM")
    (rulebase environment depth emit)
  :current
  (%io-write-term-goal
   (%io-current-output-entry rulebase)
   term options environment emit)
  :explicit
  (%io-write-term-goal
   (%io-stream-entry rulebase stream :output environment operation)
   term options environment emit))

(defun %io-write-term-goal (entry term options environment emit
                            &optional (operation (%io-operation "WRITE_TERM")))
  (let* (
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

(%define-io-dual-builtin (read (term) (term) "READ")
    (rulebase environment depth emit)
  :current
  (%io-read-term-goal
   rulebase (%io-current-input-entry rulebase)
   term '() environment emit operation)
  :explicit
  (%io-read-term-goal
   rulebase (%io-stream-entry rulebase stream :input environment operation)
   term '() environment emit operation))

(%define-io-dual-builtin (read_term (term options) (term options) "READ_TERM")
    (rulebase environment depth emit)
  :current
  (%io-read-term-goal
   rulebase (%io-current-input-entry rulebase)
   term options environment emit)
  :explicit
  (%io-read-term-goal
   rulebase (%io-stream-entry rulebase stream :input environment operation)
   term options environment emit))

(defun %io-read-term-goal (rulebase entry term options environment emit
                           &optional (operation (%io-operation "READ_TERM")))
  (let* ((parsed (%io-read-options options environment operation))
         (mode (%io-syntax-errors-mode parsed environment operation))
         (*active-char-conversions* (%rulebase-active-char-conversions rulebase)))
    (multiple-value-bind (value variables names singletons readablep)
        (%io-read-term-values entry (rulebase-operator-table rulebase) mode
                              environment operation)
      (when readablep
        (%io-unify-read-results term value parsed variables names singletons
                                environment emit)))))

(defun %io-unify-read-results (term value options variables names singletons
                               environment emit)
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
            (when names-ok
              (multiple-value-bind (singletons-environment singletons-ok)
                  (unify (%io-option "singletons" options singletons)
                         singletons names-environment)
                (when singletons-ok (funcall emit singletons-environment))))))))))

(defun %io-read-term-values (entry operator-table mode environment operation)
  (let ((input (prolog-stream-stream entry)))
    (if (eq (peek-char t input nil :eof) :eof)
        (progn
          (setf (prolog-stream-end-of-stream entry) :past)
          (values (%iso-atom "end_of_file") '() '() '() t))
        (handler-case
            (multiple-value-bind (term variables names singletons)
                (%io-parse-term-with-variables input operator-table)
              (setf (prolog-stream-end-of-stream entry) :not)
              (values term variables names singletons t))
          (prolog-parse-error (condition)
            (if (%io-option-name-p mode "error")
                (%raise-syntax-error condition environment operation)
                (values nil nil nil nil nil)))))))
