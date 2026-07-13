(in-package #:cl-prolog)

(defun %read-source-term (stream operator-table)
  (let* ((source (%read-prolog-term-source stream))
         (parser (%parser (%tokenize-prolog source operator-table) operator-table)))
    (if (eq :eof (%token-kind (%current-token parser)))
        (values nil nil)
        (let ((term (%parse-expression parser (make-hash-table :test #'equal) 0)))
          (%expect-token parser :operator ".")
          (%expect-token parser :eof)
          (values term t)))))

