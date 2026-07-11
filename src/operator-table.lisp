;;;; Persistent Prolog operator definitions.

(in-package #:cl-prolog)

(defconstant +maximum-operator-priority+ 1200)

(defparameter +operator-specifiers+ '(:fx :fy :xf :yf :xfx :xfy :yfx))

(defstruct (operator-definition
            (:constructor %make-operator-definition (name priority specifier))
            (:copier nil))
  (name nil :type symbol :read-only t)
  (priority 1 :type (integer 1 1200) :read-only t)
  (specifier :xfx :type keyword :read-only t))

(defstruct (operator-table
            (:constructor %make-operator-table (definitions))
            (:copier nil))
  (definitions '() :type list :read-only t))

(defun %valid-operator-specifier-p (specifier)
  (not (null (member specifier +operator-specifiers+ :test #'eq))))

(defun %validate-operator-designator (name priority specifier)
  (unless (symbolp name)
    (error "Operator name must be a symbol, got ~S." name))
  (unless (typep priority '(integer 0 1200))
    (error "Operator priority must be an integer from 0 through 1200, got ~S."
           priority))
  (unless (%valid-operator-specifier-p specifier)
    (error "Operator specifier must be one of ~S, got ~S."
           +operator-specifiers+ specifier)))

(defun %same-operator-key-p (definition name specifier)
  (and (eq name (operator-definition-name definition))
       (eq specifier (operator-definition-specifier definition))))

(defun %operator-table-remove (table name specifier)
  "Return TABLE without the definition identified by NAME and SPECIFIER."
  (%validate-operator-designator name 0 specifier)
  (let ((remaining
          (remove-if (lambda (definition)
                       (%same-operator-key-p definition name specifier))
                     (operator-table-definitions table))))
    (if (= (length remaining) (length (operator-table-definitions table)))
        table
        (%make-operator-table remaining))))

(defun %operator-table-define (table name priority specifier)
  "Return a new table defining NAME at PRIORITY with SPECIFIER.

Priority zero removes the matching NAME and SPECIFIER definition."
  (%validate-operator-designator name priority specifier)
  (if (zerop priority)
      (%operator-table-remove table name specifier)
      (let* ((old (operator-table-definitions table))
             (existing (find-if (lambda (definition)
                                  (%same-operator-key-p definition name specifier))
                                old)))
        (if (and existing (= priority (operator-definition-priority existing)))
            table
            (%make-operator-table
             (cons (%make-operator-definition name priority specifier)
                   (remove-if (lambda (definition)
                                (%same-operator-key-p definition name specifier))
                              old)))))))

(defun %operator-specifier-rank (specifier)
  (or (position specifier +operator-specifiers+ :test #'eq)
      (error "Unknown operator specifier ~S." specifier)))

(defun %operator-definition-less-p (left right)
  (let ((left-priority (operator-definition-priority left))
        (right-priority (operator-definition-priority right)))
    (cond
      ((/= left-priority right-priority) (< left-priority right-priority))
      ((/= (%operator-specifier-rank (operator-definition-specifier left))
           (%operator-specifier-rank (operator-definition-specifier right)))
       (< (%operator-specifier-rank (operator-definition-specifier left))
          (%operator-specifier-rank (operator-definition-specifier right))))
      (t
       (let ((left-package (symbol-package (operator-definition-name left)))
             (right-package (symbol-package (operator-definition-name right))))
         (let ((left-package-name (and left-package (package-name left-package)))
               (right-package-name (and right-package (package-name right-package))))
           (if (equal left-package-name right-package-name)
               (string< (symbol-name (operator-definition-name left))
                        (symbol-name (operator-definition-name right)))
               (string< (or left-package-name "")
                        (or right-package-name "")))))))))

(defun %operator-table-current (table)
  "Return a fresh, deterministically ordered list of TABLE's definitions."
  (stable-sort (copy-list (operator-table-definitions table))
               #'%operator-definition-less-p))

(defun %operator-table-find (table name &optional specifier)
  "Return definitions for NAME, optionally restricted to SPECIFIER."
  (unless (symbolp name)
    (error "Operator name must be a symbol, got ~S." name))
  (when specifier
    (unless (%valid-operator-specifier-p specifier)
      (error "Operator specifier must be one of ~S, got ~S."
             +operator-specifiers+ specifier)))
  (stable-sort
   (remove-if-not
    (lambda (definition)
      (and (eq name (operator-definition-name definition))
           (or (null specifier)
               (eq specifier (operator-definition-specifier definition)))))
    (operator-table-definitions table))
   #'%operator-definition-less-p))

(defparameter +standard-operator-declarations+
  '((1200 :xfx |:-|) (1200 :xfx |-->|) (1200 :fx |:-|) (1200 :fx |?-|)
    (1100 :xfy |;|) (1050 :xfy ->) (1050 :xfy *->) (1000 :xfy |,|)
    (900 :fy |\\+|)
    (700 :xfx =) (700 :xfx |\\=|) (700 :xfx ==) (700 :xfx |\\==|)
    (700 :xfx |@<|) (700 :xfx |@=<|) (700 :xfx |@>|) (700 :xfx |@>=|)
    (700 :xfx |=..|) (700 :xfx is) (700 :xfx |=:=|)
    (700 :xfx |=\\=|) (700 :xfx <) (700 :xfx =<) (700 :xfx >) (700 :xfx >=)
    (500 :yfx +) (500 :yfx -)
    (400 :yfx *) (400 :yfx /) (400 :yfx //) (400 :yfx div)
    (400 :yfx mod) (400 :yfx rem)
    (200 :xfx **) (200 :xfy ^) (200 :fy +) (200 :fy -)))

(defun %make-standard-operator-table ()
  (reduce (lambda (table declaration)
            (destructuring-bind (priority specifier name) declaration
              (%operator-table-define table name priority specifier)))
          +standard-operator-declarations+
          :initial-value (%make-operator-table '())))

(defparameter *standard-operator-table* (%make-standard-operator-table))
