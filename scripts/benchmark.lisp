#!/usr/bin/env sbcl --script

(require :asdf)

(defpackage #:cl-prolog.benchmark-script
  (:use #:cl))

(in-package #:cl-prolog.benchmark-script)

(defparameter *output-formats* '("text" "json"))

(defun script-path ()
  (or *load-truename*
      (error "Cannot determine script path from *LOAD-TRUENAME*.")))

(defun repo-root ()
  (uiop:ensure-directory-pathname
   (merge-pathnames "../" (uiop:pathname-directory-pathname (script-path)))))

(asdf:load-asd (merge-pathnames "cl-prolog.asd" (repo-root)))
(asdf:load-system :cl-prolog/benchmark)

(defun project-version ()
  (asdf:component-version (asdf:find-system :cl-prolog)))

(defun argv ()
  (uiop:command-line-arguments))

(defun print-version (&optional (stream *standard-output*))
  (format stream "cl-prolog ~A~%" (project-version)))

(defun usage (&optional (stream *standard-output*))
  (format stream "Usage: sbcl --script scripts/benchmark.lisp [--scenario NAME] [--iterations N] [--json]~%")
  (format stream "       sbcl --script scripts/benchmark.lisp --help~%")
  (format stream "       sbcl --script scripts/benchmark.lisp --version~%")
  (format stream "~%")
  (format stream "Scenarios:~%")
  (dolist (scenario (cl-prolog.benchmark:available-scenarios))
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
  (sb-ext:exit :code 2))

(defun parse-positive-integer (raw-value)
  (let ((*read-eval* nil))
    (handler-case
        (let ((value (read-from-string raw-value)))
          (if (and (integerp value) (plusp value))
              value
              (usage-error "Expected a positive integer, got: ~A" raw-value)))
      (error ()
        (usage-error "Expected a positive integer, got: ~A" raw-value)))))

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
                (sb-ext:exit :code 0))
               ((string= arg "--version")
                (print-version)
                (sb-ext:exit :code 0))
               (t
                (usage-error "Unknown argument: ~A" arg))))
    (let ((requested-scenarios
            (if scenarios
                (nreverse scenarios)
                (cl-prolog.benchmark:available-scenarios))))
      (dolist (scenario requested-scenarios)
        (unless (member scenario
                        (cl-prolog.benchmark:available-scenarios)
                        :test #'string=)
          (usage-error "Unknown benchmark scenario: ~A" scenario)))
      (list :scenarios requested-scenarios
            :iterations iterations
            :output-format output-format))))

(defun json-escaped-string (string)
  (with-output-to-string (out)
    (loop for ch across string
          do (case ch
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Backspace (write-string "\\b" out))
               (#\FormFeed (write-string "\\f" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t
                (if (< (char-code ch) 32)
                    (format out "\\u~4,'0X" (char-code ch))
                    (write-char ch out)))))))

(defun write-json-string (stream string)
  (format stream "\"~A\"" (json-escaped-string string)))

(defun write-json-result (stream result)
  (format stream "{")
  (write-json-string stream "scenario")
  (format stream ": ")
  (write-json-string stream (getf result :scenario))
  (format stream ", ")
  (write-json-string stream "ok")
  (format stream ": true, ")
  (write-json-string stream "iterations")
  (format stream ": ~D, " (getf result :iterations))
  (write-json-string stream "total_ns")
  (format stream ": ~D, " (getf result :total-ns))
  (write-json-string stream "avg_ns")
  (format stream ": ~D, " (getf result :avg-ns))
  (write-json-string stream "last_result")
  (format stream ": ")
  (write-json-string stream (getf result :last-result))
  (format stream "}"))

(defun emit-json-report (results iterations requested-scenarios)
  (format t "{")
  (write-json-string t "report_type")
  (format t ": ")
  (write-json-string t "benchmark")
  (format t ", ")
  (write-json-string t "project_version")
  (format t ": ")
  (write-json-string t (project-version))
  (format t ", ")
  (write-json-string t "requested_scenarios")
  (format t ": [")
  (loop for scenario in requested-scenarios
        for firstp = t then nil
        do (unless firstp
             (format t ", "))
           (write-json-string t scenario))
  (format t "], ")
  (write-json-string t "ok")
  (format t ": true, ")
  (write-json-string t "iterations")
  (format t ": ~D, " iterations)
  (write-json-string t "results")
  (format t ": [")
  (loop for result in results
        for firstp = t then nil
        do (unless firstp
             (format t ", "))
           (write-json-result t result))
  (format t "], ")
  (write-json-string t "scenario_count")
  (format t ": ~D" (length results))
  (format t "}~%"))

(defun emit-text-report (results iterations)
  (format t "cl-prolog benchmark run~%")
  (format t "iterations per scenario: ~D~%~%" iterations)
  (dolist (result results)
    (format t "[PASS] ~A~%" (getf result :scenario))
    (format t "  total_ns: ~D~%" (getf result :total-ns))
    (format t "  avg_ns:   ~D~%" (getf result :avg-ns))
    (format t "  last:     ~A~%~%" (getf result :last-result))))

(defun main ()
  (handler-case
      (let* ((options (parse-args (argv)))
             (scenarios (getf options :scenarios))
             (iterations (getf options :iterations))
             (output-format (getf options :output-format))
             (results (cl-prolog.benchmark:run-benchmark-suite scenarios iterations)))
        (if (string= output-format "json")
            (emit-json-report results iterations scenarios)
            (emit-text-report results iterations))
        (sb-ext:exit :code 0))
    (error (condition)
      (format *error-output* "~A~%" condition)
      (sb-ext:exit :code 1))))

(main)
