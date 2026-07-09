(in-package #:fx.prolog)

(defstruct (fact (:constructor make-fact (&key predicate (args '())))
                 (:copier nil))
  (predicate nil :type symbol :read-only t)
  (args '() :type list :read-only t))

(defstruct (rule (:constructor make-rule (&key (head '()) (body '())))
                 (:copier nil))
  (head '() :type list :read-only t)
  (body '() :type list :read-only t))

(defstruct (rulebase (:constructor make-rulebase (&key (facts '()) (rules '())))
                     (:copier nil))
  (facts '() :type list)
  (rules '() :type list))

(defparameter *global-rulebase* (make-rulebase)
  "Global rule registry used by DEF-RULE and QUERY-ALL.")

(defvar *prolog-rules* (make-hash-table :test 'eq)
  "cl-cc/prolog-compatible global rule index keyed by predicate symbol.")

(defun make-empty-rule-knowledge-base ()
  (make-rulebase))

(defun clear-global-rulebase! ()
  (setf (rulebase-facts *global-rulebase*) '()
        (rulebase-rules *global-rulebase*) '())
  (clrhash *prolog-rules*)
  *global-rulebase*)

(defun register-rule! (rulebase rule)
  (push rule (rulebase-rules rulebase))
  rulebase)

(defun assert-fact! (rulebase fact)
  (push fact (rulebase-facts rulebase))
  rulebase)

(defun assert-rule! (rulebase rule)
  (push rule (rulebase-rules rulebase))
  rulebase)

(defun prolog-compound-form-parts (form)
  (unless (and (consp form) (symbolp (first form)))
    (error "Invalid Prolog compound form: ~S" form))
  (values (first form) (rest form)))

(defun register-prolog-rule (head body)
  (multiple-value-bind (predicate args)
      (prolog-compound-form-parts head)
    (declare (ignore args))
    (let ((rule (make-rule :head head :body body)))
      (push rule (gethash predicate *prolog-rules*))
      rule)))

(defparameter *peephole-copy-prop-rules*
  '(((:const ?src ?val) (:move ?dst ?src) ((:const ?dst ?val)))
    ((:jump ?lbl) (:label ?lbl) ((:label ?lbl)))
    ((:const ?r ?_v1) (:const ?r ?v2) ((:const ?r ?v2)))
    ((:move ?mid ?src) (:move ?dst ?mid) ((:move ?mid ?src) (:move ?dst ?src))))
  "Copy-propagation peephole rules encoded as data.")

(defparameter *peephole-arithmetic-rules*
  '(((:add ?dst ?src 0) ?next ((:move ?dst ?src) ?next))
    ((:add ?dst 0 ?src) ?next ((:move ?dst ?src) ?next))
    ((:sub ?dst ?src 0) ?next ((:move ?dst ?src) ?next))
    ((:sub ?dst 0 ?src) ?next ((:neg ?dst ?src) ?next))
    ((:sub ?dst ?src ?src) ?next ((:const ?dst 0) ?next))
    ((:mul ?dst ?src 1) ?next ((:move ?dst ?src) ?next))
    ((:mul ?dst 1 ?src) ?next ((:move ?dst ?src) ?next))
    ((:mul ?dst ?src 0) ?next ((:const ?dst 0) ?next))
    ((:mul ?dst 0 ?src) ?next ((:const ?dst 0) ?next))
    ((:div ?dst ?src 1) ?next ((:move ?dst ?src) ?next))
    ((:logand ?dst ?src -1) ?next ((:move ?dst ?src) ?next))
    ((:logand ?dst -1 ?src) ?next ((:move ?dst ?src) ?next))
    ((:logand ?dst ?src 0) ?next ((:const ?dst 0) ?next))
    ((:logior ?dst ?src 0) ?next ((:move ?dst ?src) ?next))
    ((:logior ?dst 0 ?src) ?next ((:move ?dst ?src) ?next))
    ((:logior ?dst ?src -1) ?next ((:const ?dst -1) ?next))
    ((:logxor ?dst ?src 0) ?next ((:move ?dst ?src) ?next))
    ((:eq ?dst ?src ?src) ?next ((:const ?dst 1) ?next))
    ((:gt ?dst ?src ?src) ?next ((:const ?dst 0) ?next))
    ((:le ?dst ?src ?src) ?next ((:const ?dst 1) ?next))
    ((:logand ?dst ?src ?src) ?next ((:move ?dst ?src) ?next))
    ((:logior ?dst ?src ?src) ?next ((:move ?dst ?src) ?next))
    ((:logxor ?dst ?src ?src) ?next ((:const ?dst 0) ?next))
    ((:num-eq ?dst ?src ?src) ?next ((:const ?dst 1) ?next))
    ((:lt ?dst ?src ?src) ?next ((:const ?dst 0) ?next))
    ((:ge ?dst ?src ?src) ?next ((:const ?dst 1) ?next)))
  "Arithmetic and comparison peephole rules encoded as data.")

(defparameter *peephole-control-flow-rules*
  '(((:lt ?tmp ?lhs ?rhs) (:not ?dst ?tmp) ((:ge ?dst ?lhs ?rhs)))
    ((:gt ?tmp ?lhs ?rhs) (:not ?dst ?tmp) ((:le ?dst ?lhs ?rhs)))
    ((:le ?tmp ?lhs ?rhs) (:not ?dst ?tmp) ((:gt ?dst ?lhs ?rhs)))
    ((:ge ?tmp ?lhs ?rhs) (:not ?dst ?tmp) ((:lt ?dst ?lhs ?rhs)))
    ((:jump ?lbl1) (:jump ?lbl2) ((:jump ?lbl1)))
    ((:jump ?lbl) (:ret ?reg) ((:jump ?lbl)))
    ((:jump ?lbl) (:halt ?reg) ((:jump ?lbl)))
    ((:ret ?reg) (:jump ?lbl) ((:ret ?reg)))
    ((:halt ?reg) (:jump ?lbl) ((:halt ?reg)))
    ((:ret ?reg1) (:ret ?reg2) ((:ret ?reg1)))
    ((:halt ?reg1) (:halt ?reg2) ((:halt ?reg1)))
    ((:ret ?reg1) (:halt ?reg2) ((:ret ?reg1)))
    ((:halt ?reg1) (:ret ?reg2) ((:halt ?reg1))))
  "Control-flow peephole rules encoded as data.")

(defparameter *peephole-rules*
  (append *peephole-copy-prop-rules*
          *peephole-arithmetic-rules*
          *peephole-control-flow-rules*)
  "Peephole rules assembled from smaller data groups.")
