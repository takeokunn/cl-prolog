(in-package #:cl-prolog.tests)

(defmacro signals-condition (condition &body body)
  `(is (handler-case
           (progn ,@body nil)
         (,condition () t))))

(deftest io-context-registers-standard-streams ()
  (let* ((input (make-string-input-stream "hello"))
         (output (make-string-output-stream))
         (error-output (make-string-output-stream))
         (context (cl-prolog::make-prolog-io-context
                   :input input :output output :error-output error-output)))
    (is (eq input
            (cl-prolog::prolog-stream-stream
             (cl-prolog::%resolve-prolog-stream
              context (cl-prolog::%prolog-atom-symbol "user_input")
              :input nil "TEST"))))
    (is (eq output
            (cl-prolog::prolog-stream-stream
             (cl-prolog::prolog-io-context-current-output context))))
    (is (not (cl-prolog::prolog-stream-owned-p
              (cl-prolog::%resolve-prolog-stream
               context (cl-prolog::%prolog-atom-symbol "user_error")
               :output nil "TEST"))))))

(progn
(deftest io-context-resolves-handle-and-alias ()
  (let* ((context (cl-prolog::make-prolog-io-context))
         (alias (cl-prolog::%prolog-atom-symbol "temporary_output"))
         (entry (cl-prolog::%register-prolog-stream!
                 context (make-string-output-stream) :output :alias alias)))
    (is (eq entry (cl-prolog::%resolve-prolog-stream
                   context alias :output nil "TEST")))
    (is (eq entry (cl-prolog::%resolve-prolog-stream
                   context (cl-prolog::prolog-stream-handle entry)
                   :output nil "TEST")))
    (cl-prolog::%close-all-owned-prolog-streams! context)))
(deftest io-context-stream-handles-do-not-intern-symbols ()
  (let ((alias (cl-prolog::%prolog-atom-symbol "stable_stream_alias")))
    (let ((before (package-owned-symbol-count '#:cl-prolog)))
      (dotimes (index 32)
        (declare (ignorable index))
        (let* ((context (cl-prolog::make-prolog-io-context))
               (entry
                 (cl-prolog::%register-prolog-stream!
                  context (make-string-output-stream) :output :alias alias))
               (handle (cl-prolog::prolog-stream-handle entry)))
          (with-closed-io-context (context)
            (is (null (symbol-package handle)))
            (is-equal "$stream_4" (symbol-name handle))
            (is (eq entry
                    (cl-prolog::%resolve-prolog-stream
                     context handle :output nil "TEST")))
            (is (eq entry
                    (cl-prolog::%resolve-prolog-stream
                     context alias :output nil "TEST"))))))
      (is-equal before (package-owned-symbol-count '#:cl-prolog))))))

(deftest io-context-rejects-duplicate-alias ()
  (let* ((context (cl-prolog::make-prolog-io-context))
         (alias (cl-prolog::%prolog-atom-symbol "duplicate_output")))
    (cl-prolog::%register-prolog-stream!
     context (make-string-output-stream) :output :alias alias)
    (with-closed-io-context (context)
      (signals-condition cl-prolog::prolog-permission-error
        (cl-prolog::%register-prolog-stream!
         context (make-string-output-stream) :output :alias alias)))))

(deftest io-context-rejects-unknown-stream ()
  (let ((context (cl-prolog::make-prolog-io-context)))
    (signals-condition cl-prolog::prolog-existence-error
      (cl-prolog::%resolve-prolog-stream
       context (cl-prolog::%prolog-atom-symbol "missing_stream")
       nil nil "TEST"))))

(deftest io-context-rejects-wrong-direction ()
  (let* ((context (cl-prolog::make-prolog-io-context))
         (entry (cl-prolog::%register-prolog-stream!
                 context (make-string-input-stream "") :input)))
    (with-closed-io-context (context)
      (signals-condition cl-prolog::prolog-permission-error
        (cl-prolog::%resolve-prolog-stream
         context (cl-prolog::prolog-stream-handle entry)
         :output nil "TEST"))))
  (let* ((context (cl-prolog::make-prolog-io-context))
         (entry (cl-prolog::%register-prolog-stream!
                 context (make-string-output-stream) :output)))
    (with-closed-io-context (context)
      (signals-condition cl-prolog::prolog-permission-error
        (cl-prolog::%resolve-prolog-stream
         context (cl-prolog::prolog-stream-handle entry)
         :input nil "TEST")))))

(deftest io-context-rejects-malformed-registration-arguments ()
  (let ((context (cl-prolog::make-prolog-io-context)))
    (signals-condition cl-prolog::prolog-type-error
      (cl-prolog::%register-prolog-stream! context 'cl-prolog::not-a-stream :output))
    (signals-condition cl-prolog::prolog-type-error
      (cl-prolog::%register-prolog-stream!
       context (make-string-output-stream) :output :alias 123))
    (signals-condition cl-prolog::prolog-domain-error
      (cl-prolog::%register-prolog-stream!
       context (make-string-output-stream) :output :type :bogus))))

(deftest io-context-closes-only-owned-streams ()
  (let* ((standard-output (make-string-output-stream))
         (context (cl-prolog::make-prolog-io-context
                   :output standard-output
                   :error-output (make-string-output-stream)))
         (alias (cl-prolog::%prolog-atom-symbol "owned_output"))
         (stream (make-string-output-stream))
         (entry (cl-prolog::%register-prolog-stream!
                 context stream :output :alias alias)))
    (is (cl-prolog::%close-prolog-stream! context alias nil))
    (is (not (open-stream-p stream)))
    (signals-condition cl-prolog::prolog-existence-error
      (cl-prolog::%resolve-prolog-stream
       context (cl-prolog::prolog-stream-handle entry) nil nil "TEST"))
    (signals-condition cl-prolog::prolog-permission-error
      (cl-prolog::%close-prolog-stream!
       context (cl-prolog::%prolog-atom-symbol "user_output") nil))
    (is (open-stream-p standard-output))))

(deftest io-context-validates-mode ()
  (is-equal '(:read :input)
            (multiple-value-list
             (cl-prolog::%validate-prolog-stream-mode
              (cl-prolog::%prolog-atom-symbol "read") nil "TEST")))
  (is-equal '(:append :output)
            (multiple-value-list
             (cl-prolog::%validate-prolog-stream-mode
              (cl-prolog::%prolog-atom-symbol "append") nil "TEST")))
  (signals-condition cl-prolog::prolog-domain-error
    (cl-prolog::%validate-prolog-stream-mode
     (cl-prolog::%prolog-atom-symbol "update") nil "TEST")))

(deftest io-context-rejects-foreign-and-closed-stream-objects ()
  (let* ((first-context (cl-prolog::make-prolog-io-context))
         (second-context (cl-prolog::make-prolog-io-context))
         (stream (make-string-output-stream))
         (entry (cl-prolog::%register-prolog-stream!
                 first-context stream :append)))
    (with-closed-io-context (first-context)
      (is-equal :append (cl-prolog::prolog-stream-mode entry))
      (signals-condition cl-prolog::prolog-existence-error
        (cl-prolog::%resolve-prolog-stream
         second-context entry :output nil "TEST"))
      (close stream)
      (signals-condition cl-prolog::prolog-existence-error
        (cl-prolog::%resolve-prolog-stream
         first-context entry :output nil "TEST")))))
