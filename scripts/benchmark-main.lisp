(defpackage #:cl-prolog.benchmark.cli
  (:use #:cl)
  (:export #:exit-script
           #:main
           #:parse-args
           #:run-cli))

(in-package #:cl-prolog.benchmark.cli)

(defparameter *output-formats* '("text" "json"))
(defparameter *documented-scenarios*
  '("ancestor-first" "append-first" "dcg-phrase"))

(defun exit-script (code)
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (sb-ext:exit :code code))

(defun argv ()
  (cdr sb-ext:*posix-argv*))

(defun print-version (&optional (stream *standard-output*))
  (format stream "cl-prolog ~A~%" (cl-prolog.bootstrap:project-version)))

(defun available-scenarios ()
  (copy-list *documented-scenarios*))

(defun benchmark-runner ()
  (or (find-symbol "RUN-BENCHMARK-SUITE" "CL-PROLOG.BENCHMARK")
      (error "Benchmark runtime is not loaded.")))

(defun usage (&optional (stream *standard-output*))
  (format stream "Usage: sbcl --script scripts/benchmark.lisp [--scenario NAME] [--iterations N] [--json]~%")
  (format stream "       sbcl --script scripts/benchmark.lisp --help~%")
  (format stream "       sbcl --script scripts/benchmark.lisp --version~%")
  (format stream "~%")
  (format stream "Scenarios:~%")
  (dolist (scenario (available-scenarios))
    (format stream "  ~A~%" scenario))
  (format stream "~%")
  (format stream "Options:~%")
  (format stream "  --scenario NAME   Run one scenario. Repeatable. Defaults to all scenarios.~%")
  (format stream "  --iterations N    Positive integer iteration count per scenario. Default: 100.~%")
  (format stream "  --json            Emit a single JSON object with machine-readable timings.~%")
  (format stream "  --help, -h        Show this help and exit successfully.~%")
  (format stream "  --version         Print the bundled cl-prolog version and exit successfully.~%")
  (format stream "~%")
  (format stream "Exit status:~%")
  (format stream "  0  Every requested benchmark scenario completed successfully.~%")
  (format stream "  1  Benchmark execution failed after CLI validation succeeded.~%")
  (format stream "  2  Invalid CLI usage.~%")
  (format stream "~%")
  (format stream "Examples:~%")
  (format stream "  sbcl --script scripts/benchmark.lisp~%")
  (format stream "  sbcl --script scripts/benchmark.lisp --scenario ancestor-first --iterations 500~%")
  (format stream "  sbcl --script scripts/benchmark.lisp --json --scenario append-first~%"))

(defun usage-error (control &rest args)
  (apply #'format *error-output* control args)
  (format *error-output* "~%~%")
  (usage *error-output*)
  (exit-script 2))

(defun parse-positive-integer (raw-value)
  (let ((*read-eval* nil))
    (handler-case
        (let ((value (read-from-string raw-value)))
          (if (and (integerp value) (plusp value))
              value
              (usage-error "Expected a positive integer, got: ~A" raw-value)))
      (error ()
        (usage-error "Expected a positive integer, got: ~A" raw-value)))))

(defun normalize-scenarios (scenarios)
  (let ((resolved-scenarios
          (if scenarios
              (nreverse scenarios)
              (available-scenarios))))
    (dolist (scenario resolved-scenarios)
      (unless (member scenario (available-scenarios) :test #'string=)
        (usage-error "Unknown benchmark scenario: ~A" scenario)))
    resolved-scenarios))

(defun parse-args (args)
  (let ((scenarios '())
        (iterations 100)
        (output-format "text"))
    (loop while args
          for arg = (pop args)
          do (cond
               ((string= arg "--scenario")
                (unless args
                  (usage-error "Missing value for --scenario."))
                (push (pop args) scenarios))
               ((string= arg "--iterations")
                (unless args
                  (usage-error "Missing value for --iterations."))
                (setf iterations (parse-positive-integer (pop args))))
               ((string= arg "--json")
                (setf output-format "json"))
               ((member arg '("--help" "-h") :test #'string=)
                (usage)
                (exit-script 0))
               ((string= arg "--version")
                (print-version)
                (exit-script 0))
               (t
                (usage-error "Unknown argument: ~A" arg))))
    (unless (member output-format *output-formats* :test #'string=)
      (error "Unsupported output format: ~A" output-format))
    (list :scenarios (normalize-scenarios scenarios)
          :iterations iterations
          :output-format output-format)))

(defun run-cli (options)
  (emit-report (benchmark-report options)
               (getf options :output-format))
  0)

(defun main (&optional (args (argv)))
  (run-cli (parse-args args)))
