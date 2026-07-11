;;;; Release audit orchestration entry point.

(in-package #:cl-prolog.release-audit)

(defun main ()
  (setf *results* nil)
  (let ((parsed (parse-args (argv))))
    (setf *requested-checks* (getf parsed :requested-checks)
          *dry-run* (getf parsed :dry-run)
          *output-format* (getf parsed :output-format))
    (if *dry-run*
        (dolist (check *requested-checks*)
          (check-result :pass check (check-command check) "planned check"))
        (dolist (check *requested-checks*)
          (execute-check check)))
    (when (string= *output-format* "json")
      (emit-json-report))
    (final-exit-code)))
