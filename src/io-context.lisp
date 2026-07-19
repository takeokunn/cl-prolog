(in-package #:cl-prolog)

(defun %io-context-atom (name &key preserve-case)
  "Intern an I/O protocol atom without depending on the parser layer."
  (let* ((canonical-name (if preserve-case name (string-upcase name)))
         (package (find-package '#:cl-prolog)))
    (multiple-value-bind (symbol status) (find-symbol canonical-name package)
      (if (eq status :inherited)
          (intern canonical-name (find-package '#:cl-prolog.user-atoms))
          (or symbol (intern canonical-name package))))))

(defstruct (prolog-stream
             (:constructor %make-prolog-stream
                 (handle stream mode direction type alias owned-p source)))
  handle
  stream
  mode
  direction
  type
  alias
  owned-p
  source
  (end-of-stream :not))

(defstruct (prolog-io-context
             (:constructor %make-prolog-io-context
                 (streams aliases current-input current-output next-handle)))
  streams
  aliases
  current-input
  current-output
  next-handle)

(defun %validate-prolog-stream-mode (mode environment operation)
  (cond
    ((or (eq mode :input) (eq mode :read)
         (eq mode (%io-context-atom "read")))
     (values :read :input))
    ((or (eq mode :output) (eq mode :write)
         (eq mode (%io-context-atom "write")))
     (values :write :output))
    ((or (eq mode :append) (eq mode (%io-context-atom "append")))
     (values :append :output))
    (t
     (%raise-domain-error "IO_MODE" mode environment operation
                          "Stream mode must be read, write, or append"))))

(defun %next-prolog-stream-handle (context)
  (make-symbol
   (format nil "$stream_~D"
           (incf (prolog-io-context-next-handle context)))))

(defun %register-prolog-stream! (context stream mode
                                 &key alias (owned-p t) source (type :text)
                                   environment (operation "OPEN"))
  (unless (streamp stream)
    (%raise-type-error "STREAM" stream environment operation
                       "Expected a Common Lisp stream"))
  (when (and alias (not (symbolp alias)))
    (%raise-type-error "ATOM" alias environment operation
                       "Stream alias must be an atom"))
  (when (and alias (gethash alias (prolog-io-context-aliases context)))
    (%raise-permission-error "OPEN" "STREAM" alias environment operation
                             "Stream alias is already in use"))
  (multiple-value-bind (access-mode direction)
      (%validate-prolog-stream-mode mode environment operation)
    (unless (ecase direction
              (:input (input-stream-p stream))
              (:output (output-stream-p stream)))
      (%raise-permission-error (string-upcase (symbol-name direction))
                               "STREAM" (or alias stream) environment operation
                               "Common Lisp stream has the wrong direction"))
    (unless (member type '(:text :binary))
      (%raise-domain-error "STREAM_OPTION" type environment operation
                           "Stream type must be text or binary"))
    (let* ((handle (%next-prolog-stream-handle context))
           (entry (%make-prolog-stream handle stream access-mode direction
                                       type alias owned-p source)))
      (setf (gethash handle (prolog-io-context-streams context)) entry)
      (when alias
        (setf (gethash alias (prolog-io-context-aliases context)) handle))
      entry)))

(defun make-prolog-io-context
    (&key (input *standard-input*)
          (output *standard-output*)
          (error-output *error-output*))
  (let ((context (%make-prolog-io-context
                  (make-hash-table :test #'eq)
                  (make-hash-table :test #'eq)
                  nil nil 0)))
    (let ((input-entry
            (%register-prolog-stream!
             context input :input :alias (%io-context-atom "user_input")
             :owned-p nil :source :standard-input))
          (output-entry
            (%register-prolog-stream!
             context output :output :alias (%io-context-atom "user_output")
             :owned-p nil :source :standard-output)))
      (%register-prolog-stream!
       context error-output :output :alias (%io-context-atom "user_error")
       :owned-p nil :source :error-output)
      (setf (prolog-io-context-current-input context) input-entry
            (prolog-io-context-current-output context) output-entry))
    context))

(defun %copy-prolog-io-context (context)
  "Return a detached registry that refers to the same underlying streams."
  (let ((streams (make-hash-table :test #'eq))
        (aliases (make-hash-table :test #'eq))
        (entries (make-hash-table :test #'eq)))
    (maphash
     (lambda (handle entry)
       (let ((copy (copy-prolog-stream entry)))
         (setf (gethash handle streams) copy
               (gethash entry entries) copy)))
     (prolog-io-context-streams context))
    (maphash (lambda (alias handle)
               (setf (gethash alias aliases) handle))
             (prolog-io-context-aliases context))
    (%make-prolog-io-context
     streams aliases
     (gethash (prolog-io-context-current-input context) entries)
     (gethash (prolog-io-context-current-output context) entries)
     (prolog-io-context-next-handle context))))

(defun %find-prolog-stream (context designator)
  (cond
    ((prolog-stream-p designator)
     (let ((registered
             (gethash (prolog-stream-handle designator)
                      (prolog-io-context-streams context))))
       (and (eq registered designator) registered)))
    ((gethash designator (prolog-io-context-streams context)))
    (t
     (let ((handle (gethash designator (prolog-io-context-aliases context))))
       (and handle (gethash handle (prolog-io-context-streams context)))))))

(defun %forget-prolog-stream! (context entry)
  (when (eq entry (prolog-io-context-current-input context))
    (setf (prolog-io-context-current-input context)
          (%find-prolog-stream context (%io-context-atom "user_input"))))
  (when (eq entry (prolog-io-context-current-output context))
    (setf (prolog-io-context-current-output context)
          (%find-prolog-stream context (%io-context-atom "user_output"))))
  (remhash (prolog-stream-handle entry) (prolog-io-context-streams context))
  (when (prolog-stream-alias entry)
    (remhash (prolog-stream-alias entry) (prolog-io-context-aliases context))))

(defun %resolve-prolog-stream (context designator required-mode environment operation)
  (let ((entry (%find-prolog-stream context designator)))
    (unless entry
      (%raise-existence-error "STREAM" designator environment operation
                              "Unknown stream designator"))
    (when required-mode
      (let ((direction
              (nth-value 1 (%validate-prolog-stream-mode
                            required-mode environment operation))))
        (unless (eq direction (prolog-stream-direction entry))
          (%raise-permission-error
           (if (eq direction :input) "INPUT" "OUTPUT")
           "STREAM" designator environment operation
           "Stream has the wrong direction"))))
    (unless (open-stream-p (prolog-stream-stream entry))
      (%forget-prolog-stream! context entry)
      (%raise-existence-error "STREAM" designator environment operation
                              "Stream is closed"))
    entry))

(defun %close-prolog-stream! (context designator environment
                              &optional (operation "CLOSE"))
  (let ((entry (%resolve-prolog-stream context designator nil
                                       environment operation)))
    (unless (prolog-stream-owned-p entry)
      (%raise-permission-error "CLOSE" "STREAM" designator
                               environment operation
                               "Standard streams cannot be closed"))
    (unwind-protect
         (when (open-stream-p (prolog-stream-stream entry))
           (close (prolog-stream-stream entry)))
      (%forget-prolog-stream! context entry))
    t))

(defun %close-all-owned-prolog-streams! (context)
  (let ((owned '()))
    (maphash (lambda (handle entry)
               (declare (ignore handle))
               (when (prolog-stream-owned-p entry)
                 (push entry owned)))
             (prolog-io-context-streams context))
    (dolist (entry owned)
      (unwind-protect
           (when (open-stream-p (prolog-stream-stream entry))
             (close (prolog-stream-stream entry)))
        (%forget-prolog-stream! context entry))))
  t)
