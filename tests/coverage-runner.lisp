(progn
  (require :asdf)
  (require :sb-cover)
  (declaim (optimize sb-cover:store-coverage-data))
  (asdf:load-asd (truename "cl-prolog.asd"))
  (let ((source-root (namestring (truename "src/")))
        (report-directory
        (or (uiop:getenv "COVERAGE_OUTPUT") (error "COVERAGE_OUTPUT is required."))))
    (asdf:test-system :cl-prolog/tests)
    (sb-cover:report
      report-directory
      :form-mode
      :car
      :if-matches
      (lambda (name)
        (eql 0 (search source-root name))))))
