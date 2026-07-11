(in-package #:cl-prolog.verify-public-contract)

(defparameter *manifest-path* "contracts/public-contract.sexp")
(defparameter *command-timeouts*
  '(("git" . 10)
    ("tests" . 900)
    ("sbcl-script" . 20)
    ("sbcl-tests" . 900)
    ("sbcl-fresh-load" . 25)))
(defparameter *command-specs*
  '(("cl-prolog"
     :script ("--disable-debugger"
              "--script" "scripts/run-tests-noasdf.lisp")
     :fresh ("--disable-debugger"
             "--load" "scripts/bootstrap.lisp"
             "--eval" "(cl-prolog.bootstrap:load-core-sources)"
             "--eval" "(sb-ext:quit)")
     :timeout-kind "sbcl-tests")
    ("cl-prolog/examples"
     :script ("--disable-debugger"
              "--script" "examples/quick-start.lisp")
     :fresh ("--disable-debugger"
             "--script" "examples/quick-start.lisp")
     :timeout-kind "sbcl-script")
    ("cl-prolog/benchmark"
     :script ("--disable-debugger"
              "--script"
              "scripts/benchmark.lisp"
              "--json"
              "--iterations"
              "1"
              "--scenario"
              "ancestor-first")
     :fresh ("--disable-debugger"
             "--script"
             "scripts/benchmark.lisp"
             "--json"
             "--iterations"
             "1"
             "--scenario"
             "ancestor-first")
     :timeout-kind "sbcl-script")))

(defun command-timeout (kind)
  (or (cdr (assoc kind *command-timeouts* :test #'string=))
      15))

(defun sort-strings (strings)
  (sort (copy-list strings) #'string<))

(defun string-list= (left right)
  (equal (sort-strings left) (sort-strings right)))

(defun string-join (strings &optional (separator ", "))
  (with-output-to-string (out)
    (loop for string in strings
          for firstp = t then nil
          do (unless firstp
               (write-string separator out))
             (write-string string out))))

(defun plist-value (plist key)
  (let ((marker (gensym "MISSING")))
    (let ((value (getf plist key marker)))
      (if (eq value marker)
          (error "Manifest key ~A is missing." key)
          value))))

(defmacro define-manifest-accessors (&rest specs)
  `(progn
     ,@(mapcar
        (lambda (spec)
          (destructuring-bind (name key &optional default) spec
            `(defun ,name (manifest)
               ,(if default
                    `(or (getf manifest ,key) ,default)
                    `(plist-value manifest ,key)))))
        specs)))

(define-manifest-accessors
  (manifest-packages :packages)
  (manifest-asdf-systems :asdf-systems)
  (manifest-fresh-image-systems :fresh-image-systems '())
  (manifest-alias-files :alias-files '())
  (manifest-example-scripts :example-scripts)
  (manifest-core-docs :core-docs)
  (manifest-policy-files :policy-files)
  (manifest-ci-workflows :ci-workflows '())
  (manifest-stable-scripts :stable-scripts)
  (manifest-content-contracts :content-contracts '())
  (manifest-forbidden-content :forbidden-content '()))

(defun package-external-symbol-names (package-designator)
  (let ((package (find-package package-designator)))
    (unless package
      (error "Package ~A is not available." package-designator))
    (sort-strings
     (loop for symbol being the external-symbols of package
           collect (symbol-name symbol)))))

(defun package-nickname-names (package-designator)
  (let ((package (find-package package-designator)))
    (unless package
      (error "Package ~A is not available." package-designator))
    (sort-strings (mapcar #'string (package-nicknames package)))))

(defun command-spec-entry (target)
  (or (assoc target *command-specs* :test #'string=)
      (error "Unknown script target: ~A" target)))

(defun command-spec (target key)
  (let ((entry (command-spec-entry target)))
    (let ((arguments (getf (cdr entry) key))
          (timeout-kind (getf (cdr entry) :timeout-kind)))
      (unless arguments
        (error "Missing command spec ~A for target: ~A" key target))
      (values (cl-prolog.bootstrap:sbcl-program)
              (copy-list arguments)
              timeout-kind))))

(defun run-command (program arguments &key timeout)
  (cl-prolog.bootstrap:run-command-capture
   program
   arguments
   :timeout timeout
   :directory (cl-prolog.bootstrap:repo-root)))

(defun result-output-string (process-info)
  (or (getf process-info :output) ""))

(defun result-error-string (process-info)
  (or (getf process-info :error-output) ""))

(defun result-exit-code (process-info)
  (or (getf process-info :exit-code) 1))

(defun string-contains-ci-p (needle haystack)
  (and haystack
       (search needle haystack :test #'char-equal)))

(defun count-substring-occurrences (needle haystack)
  (loop with start = 0
        with count = 0
        for position = (search needle haystack :start2 start :test #'char-equal)
        while position
        do (incf count)
           (setf start (+ position (length needle)))
        finally (return count)))

(defun read-file-contents (path)
  (with-open-file (stream (cl-prolog.bootstrap:repo-file path) :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))
