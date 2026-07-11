;;;; Persistent operator table tests.

(in-package #:cl-prolog.tests)

(defun operator-summary (definition)
  (list (cl-prolog::operator-definition-priority definition)
        (cl-prolog::operator-definition-specifier definition)
        (cl-prolog::operator-definition-name definition)))

(deftest operator-table-validates-domain ()
  (dolist (specifier '(:fx :fy :xf :yf :xfx :xfy :yfx))
    (let* ((empty (cl-prolog::%make-operator-table '()))
           (table (cl-prolog::%operator-table-define empty 'sample 1 specifier)))
      (is-equal (list (list 1 specifier 'sample))
                (mapcar #'operator-summary
                        (cl-prolog::%operator-table-current table)))))
  (let ((empty (cl-prolog::%make-operator-table '())))
    (is (cl-prolog::operator-table-p
         (cl-prolog::%operator-table-define empty 'low 1 :xfx)))
    (is (cl-prolog::operator-table-p
         (cl-prolog::%operator-table-define empty 'high 1200 :xfx)))
    (signals-error (cl-prolog::%operator-table-define empty 'bad -1 :xfx))
    (signals-error (cl-prolog::%operator-table-define empty 'bad 1201 :xfx))
    (signals-error (cl-prolog::%operator-table-define empty 'bad 1 :unknown))
    (signals-error (cl-prolog::%operator-table-define empty "bad" 1 :xfx))))

(deftest operator-table-updates-are-persistent ()
  (let* ((empty (cl-prolog::%make-operator-table '()))
         (infix (cl-prolog::%operator-table-define empty 'shared 500 :yfx))
         (both (cl-prolog::%operator-table-define infix 'shared 200 :fy))
         (unchanged (cl-prolog::%operator-table-define both 'shared 500 :yfx))
         (redefined (cl-prolog::%operator-table-define both 'shared 600 :yfx))
         (removed (cl-prolog::%operator-table-define redefined 'shared 0 :fy)))
    (is-equal '() (cl-prolog::%operator-table-current empty))
    (is-equal '((500 :yfx shared))
              (mapcar #'operator-summary (cl-prolog::%operator-table-current infix)))
    (is-equal '((200 :fy shared) (500 :yfx shared))
              (mapcar #'operator-summary (cl-prolog::%operator-table-current both)))
    (is (eq both unchanged))
    (is-equal '((200 :fy shared) (600 :yfx shared))
              (mapcar #'operator-summary (cl-prolog::%operator-table-current redefined)))
    (is-equal '((600 :yfx shared))
              (mapcar #'operator-summary (cl-prolog::%operator-table-current removed)))
    (is (eq removed (cl-prolog::%operator-table-remove removed 'missing :xfx)))))

(deftest operator-table-query-and-order-are-deterministic ()
  (let* ((empty (cl-prolog::%make-operator-table '()))
         (table (cl-prolog::%operator-table-define empty 'zeta 500 :yfx))
         (table (cl-prolog::%operator-table-define table 'alpha 500 :yfx))
         (table (cl-prolog::%operator-table-define table 'alpha 500 :fx))
         (table (cl-prolog::%operator-table-define table 'omega 200 :xfy)))
    (is-equal '((200 :xfy omega) (500 :fx alpha) (500 :yfx alpha) (500 :yfx zeta))
              (mapcar #'operator-summary (cl-prolog::%operator-table-current table)))
    (is-equal '((500 :fx alpha) (500 :yfx alpha))
              (mapcar #'operator-summary
                      (cl-prolog::%operator-table-find table 'alpha)))
    (is-equal '((500 :yfx alpha))
              (mapcar #'operator-summary
                      (cl-prolog::%operator-table-find table 'alpha :yfx)))
    (let ((first (cl-prolog::%operator-table-current table)))
      (setf (rest first) nil)
      (is-equal 4 (length (cl-prolog::%operator-table-current table))))
    (signals-error (cl-prolog::%operator-table-find table "alpha"))
    (signals-error (cl-prolog::%operator-table-find table 'alpha :unknown))))

(deftest standard-operator-table-is-self-contained ()
  (let ((before (cl-prolog::%operator-table-current
                 cl-prolog::*standard-operator-table*)))
    (is-equal (length cl-prolog::+standard-operator-declarations+)
              (length before))
    (dolist (definition before)
      (cl-prolog::%operator-table-find
       cl-prolog::*standard-operator-table*
       (cl-prolog::operator-definition-name definition)
       (cl-prolog::operator-definition-specifier definition)))
    (is-equal before
              (cl-prolog::%operator-table-current
               cl-prolog::*standard-operator-table*)))
  (dolist (expected '((1200 :xfx cl-prolog::|:-|)
                       (1200 :fx cl-prolog::|:-|)
                       (1100 :xfy cl-prolog::|;|)
                       (1000 :xfy cl-prolog::|,|)
                       (900 :fy cl-prolog::|\\+|)
                       (700 :xfx cl-prolog::|=:=|)
                      (700 :xfx =) (500 :yfx +) (200 :fy -)))
    (destructuring-bind (priority specifier name) expected
      (is-equal (list expected)
                (mapcar #'operator-summary
                        (remove-if-not
                         (lambda (definition)
                           (= priority
                              (cl-prolog::operator-definition-priority definition)))
                         (cl-prolog::%operator-table-find
                          cl-prolog::*standard-operator-table* name specifier)))))))
