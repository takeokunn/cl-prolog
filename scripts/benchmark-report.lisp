(in-package #:cl-prolog.benchmark.cli)

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

(defun write-json-key (stream key)
  (write-json-string stream key)
  (format stream ": "))

(defun write-json-string-field (stream key value &key leading-comma)
  (when leading-comma
    (format stream ", "))
  (write-json-key stream key)
  (write-json-string stream value))

(defun write-json-true-field (stream key &key leading-comma)
  (when leading-comma
    (format stream ", "))
  (write-json-key stream key)
  (format stream "true"))

(defun write-json-integer-field (stream key value &key leading-comma)
  (when leading-comma
    (format stream ", "))
  (write-json-key stream key)
  (format stream "~D" value))

(defun write-json-string-list (stream strings)
  (format stream "[")
  (loop for string in strings
        for firstp = t then nil
        do (unless firstp
             (format stream ", "))
           (write-json-string stream string))
  (format stream "]"))

(defun write-json-results-list (stream results)
  (format stream "[")
  (loop for result in results
        for firstp = t then nil
        do (unless firstp
             (format stream ", "))
           (write-json-result stream result))
  (format stream "]"))

(defun emit-json-report-header (report)
  (write-json-string-field t "report_type" "benchmark")
  (write-json-string-field t "project_version"
                           (cl-prolog.bootstrap:project-version)
                           :leading-comma t)
  (format t ", ")
  (write-json-key t "requested_scenarios")
  (write-json-string-list t (getf report :scenarios))
  (write-json-true-field t "ok" :leading-comma t)
  (write-json-integer-field t "iterations"
                            (getf report :iterations)
                            :leading-comma t))

(defun emit-json-report-results (report)
  (write-json-key t "results")
  (write-json-results-list t (getf report :results))
  (write-json-integer-field t "scenario_count"
                            (length (getf report :results))
                            :leading-comma t))

(defun emit-json-report (report)
  (format t "{")
  (emit-json-report-header report)
  (format t ", ")
  (emit-json-report-results report)
  (format t "}~%"))

(defun emit-text-report (report)
  (format t "cl-prolog benchmark run~%")
  (format t "iterations per scenario: ~D~%~%"
          (getf report :iterations))
  (dolist (result (getf report :results))
    (format t "[PASS] ~A~%" (getf result :scenario))
    (format t "  total_ns: ~D~%" (getf result :total-ns))
    (format t "  avg_ns:   ~D~%" (getf result :avg-ns))
    (format t "  last:     ~A~%~%" (getf result :last-result))))

(defun benchmark-report (options)
  (let ((results
          (funcall (symbol-function (benchmark-runner))
                   (getf options :scenarios)
                   (getf options :iterations))))
    (list :scenarios (getf options :scenarios)
          :iterations (getf options :iterations)
          :results results)))

(defun emit-report (report output-format)
  (if (string= output-format "json")
      (emit-json-report report)
      (emit-text-report report)))
