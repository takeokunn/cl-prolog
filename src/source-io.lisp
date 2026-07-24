;;;; Source-stream and pathname resolution for Prolog source loading:
;;;; canonicalizing source pathnames, opening source streams, and
;;;; translating parser/IO failures into ISO source-loading errors.

(in-package #:cl-prolog)

(defvar *current-prolog-source-directory* nil)

(defvar *current-prolog-source* nil)

(defvar *current-prolog-source-record* nil)

(define-condition prolog-source-not-found (file-error) ())

(defun %resolve-prolog-source-pathname (pathname)
  (merge-pathnames pathname
                   (or *current-prolog-source-directory*
                       *default-pathname-defaults*)))

(defun %canonical-prolog-source-pathname (pathname)
  "Return PATHNAME's canonical existing identity or signal a source error."
  (let ((resolved (%resolve-prolog-source-pathname pathname)))
    (handler-case
        (truename resolved)
      (file-error ()
        (error 'prolog-source-not-found :pathname resolved)))))

(defun %call-with-prolog-source-stream (source function)
  (etypecase source
    (string
     (with-input-from-string (stream source)
       (funcall function stream)))
    (stream (funcall function source))
    (pathname
     (let* ((resolved (%resolve-prolog-source-pathname source))
            (directory (make-pathname :name nil :type nil :version nil
                                      :defaults resolved)))
       (with-open-file (stream resolved :direction :input
                                       :if-does-not-exist nil)
         (unless stream
           (error 'prolog-source-not-found :pathname resolved))
         (let ((*current-prolog-source-directory* directory))
           (funcall function stream)))))))

(defun %source-file-pathnames (term environment operation)
  "Resolve TERM, an atom or proper list of atoms, to source pathnames."
  (labels ((source-pathname (value)
             (let ((resolved (logic-substitute value environment)))
               (when (logic-var-p resolved)
                 (%raise-instantiation-error environment operation
                                             "Source must be instantiated"))
               (pathname (%io-pathname resolved environment operation))))
           (source-list (value original)
             (let ((seen (make-hash-table :test #'eq))
                   (result '())
                   (tail value)
                   (observed 0))
               (loop
                 (cond
                   ((logic-var-p tail)
                    (%raise-instantiation-error
                     environment operation "Source list must be instantiated"))
                   ((null tail)
                    (return (nreverse result)))
                   ((consp tail)
                    (when (gethash tail seen)
                      (%parser-resource-error
                       "SOURCE_LIST_CYCLE" 0 1 observed))
                    (setf (gethash tail seen) t)
                    (incf observed)
                    (push (source-pathname (car tail)) result)
                    (setf tail (cdr tail)))
                   (t
                    (%raise-type-error
                     "LIST" original environment operation
                     "Source must be a proper list of atoms")))))))
    (let ((value (logic-substitute term environment)))
      (cond
        ((logic-var-p value)
         (%raise-instantiation-error environment operation
                                     "Source must be instantiated"))
        ((null value) '())
        ((symbolp value) (list (source-pathname value)))
        ((consp value) (source-list value value))
        (t
         (%raise-type-error "ATOM" value environment operation
                            "Source must be an atom or proper list of atoms"))))))

(defmacro with-prolog-source-errors ((environment operation) &body body)
  "Translate source loading failures into operation-specific ISO errors."
  `(handler-case
       (progn ,@body)
     (prolog-source-not-found (condition)
       (%raise-existence-error
        "SOURCE_SINK"
        (make-symbol (namestring (file-error-pathname condition)))
        ,environment ,operation
        "Source file does not exist"))
     (prolog-parser-resource-error (condition)
       (%raise-parser-resource-error condition ,environment ,operation))
     (prolog-parse-error (condition)
       (%raise-syntax-error condition ,environment ,operation))))
