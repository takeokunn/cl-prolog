;;;; Unification and substitution.
;;;;
;;;; Environments are association lists mapping logic variables to terms.
;;;; They are persistent: UNIFY never mutates an environment, it extends it,
;;;; so backtracking is simply "keep using the older environment".
(in-package #:cl-prolog)

(defvar *logic-variable-ordinals* nil)

(defvar *next-logic-variable-ordinal* 0)

(defmacro %with-logic-variable-order (&body body)
  "Run BODY inside a variable-creation-order context.

An enclosing context is reused so nested queries (e.g. a builtin proving a
sub-query) keep the ordinals of variables created by their caller."
  `(if *logic-variable-ordinals* (progn
      ,@body)
    (let ((*logic-variable-ordinals* (make-hash-table :test #'eq))
          (*next-logic-variable-ordinal* 0))
      ,@body)))

(defun %register-logic-variable (variable)
  "Assign VARIABLE its stable creation ordinal and return VARIABLE."
  (unless *logic-variable-ordinals*
    (error "Logic variables require an active ordering context."))
  (multiple-value-bind (ordinal present-p) (gethash variable *logic-variable-ordinals*)
    (declare (ignore ordinal))
    (unless present-p
      (setf (gethash variable *logic-variable-ordinals*) (prog1
          *next-logic-variable-ordinal*
          (incf *next-logic-variable-ordinal*)))))
  variable)

(defun %logic-variable-ordinal (variable)
  "Return VARIABLE's registered ordinal."
  (multiple-value-bind (ordinal present-p) (gethash variable *logic-variable-ordinals*)
    (unless present-p
      (error "Unregistered logic variable ~S." variable))
    ordinal))

(defun logic-var-p (term)
  "Return true when TERM is a logic variable rather than a dedicated Prolog atom."
  (and
    (symbolp term)
    (not (keywordp term))
    (not (eq (symbol-package term) (find-package '#:cl-prolog.user-atoms)))
    (plusp (length (symbol-name term)))
    (char= (char (symbol-name term) 0) #\?)))

(defun fresh-logic-variable (&optional (prefix "?VAR"))
  "Return a fresh, never-before-seen logic variable."
  (let ((variable (gensym prefix)))
    (if *logic-variable-ordinals* (%register-logic-variable variable)
      variable)))

(progn
  (defconstant +environment-index-overlay-threshold+ 8)
  (defstruct (%environment-index
      (:constructor %make-environment-index-object
        (table overlay overlay-length next-binding-rank)))
    (table (make-hash-table :test (function eq)) :type hash-table)
    (overlay nil :type list)
    (overlay-length 0 :type (integer 0 *))
    (next-binding-rank -1 :type integer))
  (defun %environment-index-entry (variable index)
    "Return the newest entry for VARIABLE from INDEX."
    (dolist (binding (%environment-index-overlay index)
             (gethash variable (%environment-index-table index)))
      (when (eq variable (car binding))
        (return (values (cdr binding) t)))))
  (defun %make-environment-index (environment &optional (additional-capacity 0))
    "Index ENVIRONMENT by variable identity while preserving first-binding wins."
    (check-type additional-capacity (integer 0 *))
    (let ((table
            (make-hash-table
              :test
              (function eq)
              :size
              (+ (length environment) additional-capacity)))
          (rank 0))
      (dolist (binding environment)
        (multiple-value-bind (entry present-p)
            (gethash (car binding) table)
          (declare (ignore entry))
          (unless present-p
            (setf (gethash (car binding) table)
                  (cons (cdr binding) rank))))
        (incf rank))
      (%make-environment-index-object table nil 0 -1)))
  (defun %copy-environment-index (index)
    "Return a writable index object sharing the immutable contents of INDEX."
    (%make-environment-index-object
      (%environment-index-table index)
      (%environment-index-overlay index)
      (%environment-index-overlay-length index)
      (%environment-index-next-binding-rank index)))
  (defun %compact-environment-index (index)
    "Merge the bounded overlay into a new immutable base table."
    (if (zerop (%environment-index-overlay-length index))
        index
        (let ((table
                (make-hash-table
                  :test
                  (function eq)
                  :size
                  (+ (hash-table-count (%environment-index-table index))
                     (%environment-index-overlay-length index)))))
          (maphash
            (lambda (variable entry)
              (setf (gethash variable table) entry))
            (%environment-index-table index))
          (labels ((install-oldest-first (overlay)
                     (when overlay
                       (install-oldest-first (cdr overlay))
                       (let ((binding (car overlay)))
                         (setf (gethash (car binding) table)
                               (cdr binding))))))
            (install-oldest-first (%environment-index-overlay index)))
          (%make-environment-index-object
            table
            nil
            0
            (%environment-index-next-binding-rank index)))))
  (defun %extend-environment-index (index bindings)
    "Return INDEX extended by BINDINGS ordered oldest to newest."
    (let ((extended (%copy-environment-index index)))
      (dolist (binding bindings extended)
        (push
          (cons
            (car binding)
            (cons
              (cdr binding)
              (%environment-index-next-binding-rank extended)))
          (%environment-index-overlay extended))
        (incf (%environment-index-overlay-length extended))
        (decf (%environment-index-next-binding-rank extended))
        (when (= (%environment-index-overlay-length extended)
                 +environment-index-overlay-threshold+)
          (setf extended (%compact-environment-index extended))))))
  (defun %environment-index-after-bindings
      (bindings parent-bindings parent-index)
    "Reuse PARENT-INDEX for an unchanged environment or extend a prepended prefix."
    (if (eq bindings parent-bindings)
        parent-index
        (let ((reversed-prefix (quote ()))
              (tail bindings))
          (loop until (eq tail parent-bindings)
                do (unless (consp tail)
                     (return-from
                       %environment-index-after-bindings
                       (%make-environment-index bindings)))
                   (push (car tail) reversed-prefix)
                   (setf tail (cdr tail)))
          (%extend-environment-index parent-index reversed-prefix))))
  (defun %alias-cycle-representative (start index)
    "Choose the earliest effective binding in the alias cycle containing START."
    (let* ((entry (%environment-index-entry start index))
           (representative start)
           (best-rank (cdr entry))
           (term (car entry)))
      (loop until (eq term start)
            for term-entry = (%environment-index-entry term index)
            when (< (cdr term-entry) best-rank)
              do (setf representative term
                       best-rank (cdr term-entry))
            do (setf term (car term-entry))
            finally (return representative))))
  (defun %walk-term-indexed (term index seen)
    (declare (ignore seen))
    (labels ((advance (node)
               (if (logic-var-p node)
                   (multiple-value-bind (entry present-p)
                       (%environment-index-entry node index)
                     (if present-p
                         (values (car entry) t)
                         (values node nil)))
                   (values node nil))))
      (let ((slow term)
            (fast term))
        (loop
          (multiple-value-bind (next bound-p) (advance slow)
            (unless bound-p
              (return next))
            (setf slow next))
          (loop repeat 2
                do (multiple-value-bind (next bound-p) (advance fast)
                     (unless bound-p
                       (return-from %walk-term-indexed fast))
                     (setf fast next)))
          (when (eq slow fast)
            (return (%alias-cycle-representative slow index)))))))
  (defun %walk-term (term env)
    "Chase TERM through ENV until it is unbound or not a variable."
    (%walk-term-indexed term (%make-environment-index env) nil)))

(progn
  (defun %occurs-p-indexed (var term index walk-seen)
    (let ((seen nil))
      (labels ((occurs-p (node)
                 (let ((resolved (%walk-term-indexed node index walk-seen)))
              (cond
                ((eq var resolved) t)
                ((not (consp resolved)) nil)
                ((and seen (gethash resolved seen)) nil)
                (t
                  (unless seen
                    (setf seen (make-hash-table :test (function eq))))
                  (setf (gethash resolved seen) t)
                  (or (occurs-p (car resolved)) (occurs-p (cdr resolved))))))))
        (occurs-p term))))
  (defun %occurs-p (var term env)
    (%occurs-p-indexed var term (%make-environment-index env) nil)))

(defun %unify-indexed (left right env base-index &optional index-owned-p)
  "Unify using BASE-INDEX, returning environment, success flag, and new index. INDEX-OWNED-P permits in-place extension of a caller-owned transient index."
  (let ((index base-index)
        (copied-p index-owned-p)
        (seen-pairs nil)
        (walk-seen nil))
    (labels ((ensure-writable-index ()
               (unless copied-p
                 (setf index (%copy-environment-index index)
                       copied-p t)))
             (extend-environment (variable term environment)
               (ensure-writable-index)
               (push
                 (cons
                   variable
                   (cons
                     term
                     (%environment-index-next-binding-rank index)))
                 (%environment-index-overlay index))
               (incf (%environment-index-overlay-length index))
               (decf (%environment-index-next-binding-rank index))
               (when (= (%environment-index-overlay-length index)
                        +environment-index-overlay-threshold+)
                 (setf index (%compact-environment-index index)))
               (acons variable term environment))
             (seen-pair-p (left right)
               (and seen-pairs
                    (member
                      right
                      (gethash left seen-pairs)
                      :test
                      (function eq))))
             (remember-pair (left right)
               (unless seen-pairs
                 (setf seen-pairs
                       (make-hash-table :test (function eq))))
               (push right (gethash left seen-pairs)))
             (unify-terms (left right environment)
               (setf left (%walk-term-indexed left index walk-seen)
                     right (%walk-term-indexed right index walk-seen))
               (cond
                 ((eq left right) (values environment t))
                 ((logic-var-p left)
                  (if (%occurs-p-indexed left right index walk-seen)
                      (values nil nil)
                      (values
                        (extend-environment left right environment)
                        t)))
                 ((logic-var-p right)
                  (unify-terms right left environment))
                 ((and (consp left) (consp right))
                  (if (seen-pair-p left right)
                      (values environment t)
                      (progn
                        (remember-pair left right)
                        (multiple-value-bind (extended ok)
                            (unify-terms
                              (car left)
                              (car right)
                              environment)
                          (if ok
                              (unify-terms
                                (cdr left)
                                (cdr right)
                                extended)
                              (values nil nil))))))
                 ((and
                    (symbolp left)
                    (symbolp right)
                    (string=
                      (symbol-name left)
                      (symbol-name right)))
                  (values environment t))
                 ((equal left right) (values environment t))
                 (t (values nil nil)))))
      (multiple-value-bind (extended ok)
          (unify-terms left right env)
        (if ok
            (values extended t index)
            (values nil nil base-index))))))

(defun unify (left right &optional (env (quote ())))
  "Unify LEFT and RIGHT against ENV.

Returns (VALUES EXTENDED-ENV T) on success and (VALUES NIL NIL) on failure."
  (multiple-value-bind (extended ok index) (%unify-indexed left right env (%make-environment-index env 1) t)
    (declare (ignore index))
    (values extended ok)))

(defun %logic-substitute-indexed (template index)
  "Apply INDEX to TEMPLATE while preserving dotted and cyclic structure."
  (let ((root (%walk-term-indexed template index nil)))
    (if (not (consp root))
        root
        (let ((copies (make-hash-table :test (function eq))))
          (labels ((copy-resolved-term (resolved)
                     (if (consp resolved)
                         (or (gethash resolved copies)
                             (let ((copy (cons nil nil)))
                               (setf (gethash resolved copies) copy
                                     (car copy)
                                     (substitute-term (car resolved))
                                     (cdr copy)
                                     (substitute-term (cdr resolved)))
                               copy))
                         resolved))
                   (substitute-term (term)
                     (copy-resolved-term
                       (%walk-term-indexed term index nil))))
            (copy-resolved-term root))))))

(defun logic-substitute (template env)
  "Recursively apply ENV to TEMPLATE, preserving dotted structure."
  (%logic-substitute-indexed template (%make-environment-index env)))

(defun %collect-variables (term)
  "Return the logic variables of TERM in first-appearance order."
  (let ((seen (make-hash-table :test #'eq))
        (seen-conses (make-hash-table :test #'eq))
        (variables '()))
    (labels ((walk (node)
               (cond
            ((logic-var-p node)
              (when *logic-variable-ordinals*
                (%register-logic-variable node))
              (unless (gethash node seen)
                (setf (gethash node seen) t)
                (push node variables)))
            ((consp node)
              (unless (gethash node seen-conses)
                (setf (gethash node seen-conses) t)
                (walk (car node))
                (walk (cdr node)))))))
      (walk term))
    (nreverse variables)))

(progn
  (defconstant +freshening-map-threshold+ 12)
  (defstruct (%freshening-map (:constructor %make-freshening-map ()))
    (entries (make-array (* 2 +freshening-map-threshold+) :initial-element nil))
    (count 0 :type fixnum)
    (table nil :type (or null hash-table)))
  (defun %freshening-map-lookup (key mapping)
    (if (hash-table-p mapping)
        (gethash key mapping)
        (let ((table (%freshening-map-table mapping)))
          (if table
              (gethash key table)
              (let ((entries (%freshening-map-entries mapping))
                    (count (%freshening-map-count mapping)))
                (loop for index below count
                      for offset = (* index 2)
                      when (eq key (svref entries offset))
                        do (return (values (svref entries (1+ offset)) t))
                      finally (return (values nil nil))))))))
  (defun %freshening-map-insert (key value mapping)
    (cond
      ((hash-table-p mapping)
       (setf (gethash key mapping) value))
      ((%freshening-map-table mapping)
       (setf (gethash key (%freshening-map-table mapping)) value))
      (t
       (let ((count (%freshening-map-count mapping))
             (entries (%freshening-map-entries mapping)))
         (if (< count +freshening-map-threshold+)
             (progn
               (setf (svref entries (* count 2)) key
                     (svref entries (1+ (* count 2))) value)
               (incf (%freshening-map-count mapping))
               value)
             (let ((table (make-hash-table
                            :test (function eq)
                            :size (* 2 +freshening-map-threshold+))))
               (dotimes (index count)
                 (let ((offset (* index 2)))
                   (setf (gethash (svref entries offset) table)
                         (svref entries (1+ offset)))))
               (setf (%freshening-map-table mapping) table
                     (%freshening-map-entries mapping) nil
                     (gethash key table) value)
               value))))))
  (defun %freshen-term
      (term table &optional (copies (make-hash-table :test (function eq))))
    "Copy TERM, replacing each logic variable via TABLE with a fresh one.
COPIES preserves cons identity and cycles across calls that share it."
    (labels ((freshen (node)
               (cond
                 ((logic-var-p node)
                  (multiple-value-bind (fresh present-p)
                      (%freshening-map-lookup node table)
                    (if present-p
                        fresh
                        (%freshening-map-insert
                          node (fresh-logic-variable "?FRESH") table))))
                 ((consp node)
                  (multiple-value-bind (copy present-p)
                      (%freshening-map-lookup node copies)
                    (if present-p
                        copy
                        (let ((copy (cons nil nil)))
                          (%freshening-map-insert node copy copies)
                          (setf (car copy) (freshen (car node))
                                (cdr copy) (freshen (cdr node)))
                          copy))))
                 (t node))))
      (freshen term))))

(defun %term-has-variables-p (term)
  "True when TERM contains at least one logic variable."
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((has-variables-p (node)
               (cond
            ((logic-var-p node) t)
            ((not (consp node)) nil)
            ((gethash node seen) nil)
            (t
              (setf (gethash node seen) t)
              (or (has-variables-p (car node)) (has-variables-p (cdr node)))))))
      (has-variables-p term))))

(defun %freshen-clause (clause)
  "Return CLAUSE with all logic variables consistently renamed to fresh ones."
  (let ((mapping (%make-freshening-map)))
    (make-clause
      (%freshen-term (clause-head clause) mapping mapping)
      (mapcar
        (lambda (goal)
          (%freshen-term goal mapping mapping))
        (clause-body clause)))))
