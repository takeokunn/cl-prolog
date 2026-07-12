(in-package #:cl-prolog.tests)

(defun %temporary-prolog-pathname ()
  (merge-pathnames
   (make-pathname :name (format nil ".cl-prolog-source-~A" (gensym))
                  :type "pl")
   (truename ".")))

(defun %call-with-temporary-prolog-files (sources function)
  (let ((pathnames (loop repeat (length sources)
                         collect (%temporary-prolog-pathname))))
    (unwind-protect
         (progn
           (loop for pathname in pathnames
                 for source in sources
                 do (with-open-file (output pathname
                                            :direction :output
                                            :if-exists :supersede)
                      (write-string source output)))
           (funcall function pathnames))
      (dolist (pathname pathnames)
        (when (probe-file pathname)
          (delete-file pathname))))))

(defmacro with-temporary-prolog-files ((&rest bindings) &body body)
  `(%call-with-temporary-prolog-files
    (list ,@(mapcar #'second bindings))
    (lambda (pathnames)
      (destructuring-bind ,(mapcar #'first bindings) pathnames
        ,@body))))

(defun %prolog-path-atom (pathname)
  (with-output-to-string (output)
    (write-char #\' output)
    (loop for character across (namestring pathname)
          do (write-char character output)
             (when (char= character #\')
               (write-char character output)))
    (write-char #\' output)))

(defun %prolog-path-list (pathnames)
  (format nil "[~{~A~^, ~}]" (mapcar #'%prolog-path-atom pathnames)))

(defun %call-with-source-medium (medium source function)
  (ecase medium
    (:string (funcall function source))
    (:stream
     (with-input-from-string (stream source)
       (let ((result (funcall function stream)))
         (is (open-stream-p stream))
         result)))
    (:pathname
     (let ((pathname (%temporary-prolog-pathname)))
       (unwind-protect
            (progn
              (with-open-file (output pathname
                                      :direction :output
                                      :if-exists :supersede)
                (write-string source output))
              (funcall function pathname))
         (when (probe-file pathname)
           (delete-file pathname)))))))

(defun %consult-through-medium (medium source &optional (rulebase (make-rulebase)))
  (%call-with-source-medium
   medium source
   (lambda (input)
     (consult-prolog input rulebase))))

(defun %source-query-succeeds-p (rulebase source)
  (prolog-succeeds-p
   rulebase
   (read-prolog-term source (cl-prolog::rulebase-operator-table rulebase))))

(defun %source-list-query-succeeds-p (rulebase sources query-format)
  "Check that QUERY-FORMAT succeeds when SOURCES are rendered as a Prolog list."
  (%source-query-succeeds-p
   rulebase
   (format nil query-format (%prolog-path-list sources))))

(defmacro %source-queries-succeed-p (rulebase &rest sources)
  "Assert that each source query succeeds against RULEBASE."
  `(progn
     ,@(mapcar (lambda (source)
                 `(is (%source-query-succeeds-p ,rulebase ,source)))
               sources)))

(defmacro %source-queries-fail-p (rulebase &rest sources)
  "Assert that each source query fails against RULEBASE."
  `(progn
     ,@(mapcar (lambda (source)
                 `(is (not (%source-query-succeeds-p ,rulebase ,source))))
               sources)))

(defun %source-predicate-defined-p (rulebase name)
  (find name (rulebase-visible-clauses rulebase)
        :key (lambda (clause)
               (car (clause-head clause)))
        :test #'eq))
