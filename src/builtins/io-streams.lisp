;;;; ISO stream read/write builtins.

(in-package #:cl-prolog)

(defun %io-at-end-p (entry environment operation)
  (if (eq (prolog-stream-end-of-stream entry) :past)
      t
      (let ((at-end-p
              (ecase (prolog-stream-type entry)
                (:text
                 (eq (peek-char nil (prolog-stream-stream entry) nil :eof) :eof))
                (:binary
                 (= -1 (%io-read-byte entry environment operation :peek t))))))
        (setf (prolog-stream-end-of-stream entry)
              (if at-end-p :at :not))
        at-end-p)))

(%define-io-dual-builtin (at_end_of_stream () () "AT_END_OF_STREAM")
    (rulebase environment depth emit)
  :current
  (when (%io-at-end-p
         (%io-current-input-entry rulebase)
         environment operation)
    (funcall emit environment))
  :explicit
  (when (%io-at-end-p
         (%io-stream-entry rulebase stream :input environment operation)
         environment operation)
    (funcall emit environment)))
