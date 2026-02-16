(in-package :moonli)

(esrap:defrule quoted-expression
    (and #\$ (or atomic-expression expr:symbol))
  (:function (lambda (expr)
               (quote-moonli-expression (second expr)))))


(defun quote-moonli-expression (expr)
  (if (listp expr)
      (%quote-moonli-expression (car expr) (cdr expr))
      `(cl:quote ,expr)))

(defgeneric %quote-moonli-expression (car cdr))

(defmethod %quote-moonli-expression (car cdr)
  `(cl:quote (,car ,@cdr)))

(defmethod %quote-moonli-expression ((car (eql 'list)) cdr)
  `(list ,@(mapcar #'quote-moonli-expression cdr)))

(defmethod %quote-moonli-expression ((car (eql 'fill-hash-table)) cdr)
  `(fill-hash-table
    ,@(mapcar (lambda (key-val)
                (list (quote-moonli-expression (first key-val))
                      (quote-moonli-expression (second key-val))))
              cdr)))

(defmethod %quote-moonli-expression ((car (eql 'fill-hash-set)) cdr)
  `(fill-hash-set ,@(mapcar #'quote-moonli-expression cdr)))

