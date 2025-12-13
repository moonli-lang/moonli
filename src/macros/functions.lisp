(in-package :moonli)

(5am:in-suite :moonli)

(esrap:defrule lambda-parameter
    (or (and expr:symbol *whitespace #\= *whitespace moonli-expression)
        expr:symbol)
  (:function (lambda (expr)
               (if (symbolp expr)
                   expr
                   (optima:ematch expr
                     ((list parameter _ _ _ default)
                      (list parameter default)))))))

(esrap:defrule lambda-parameter-list
    (or (and #\( *whitespace #\))
        (and #\(
             *whitespace
             lambda-parameter
             *whitespace
             (* (and #\, *whitespace lambda-parameter *whitespace))
             #\))
        (and #\(
             (+ (and *whitespace
                     lambda-parameter
                     *whitespace #\, *whitespace))
             #\)))
  (:function (lambda (expr)
               (if (null (cdddr expr)) ; length = 3, first or last
                   (mapcar #'second (second expr))
                   (cons (third expr) ; middle
                         (mapcar #'third (fifth expr)))))))

(define-moonli-macro defun
  ((name good-symbol)
   (_ *whitespace)
   (lambda-list lambda-parameter-list)
   (_ *whitespace)
   (_ #\:)
   (body (esrap:? moonli)))
  `(defun ,name ,lambda-list
     ,@(rest body)))


(def-test defun (macro-call)
  (:moonli "defun our-identity(x): x end"
   :lisp (defun our-identity (x) x))
  (:moonli "defun add (&rest, args):
 args
end defun"
   :lisp (defun add (&rest args) args))
  (:expr moonli-expression
   :moonli "defun add(args):
  if null(args):
    0
  else:
    first(args) + add(rest(args))
  end if
end"
   :lisp (defun add (args)
           (cond ((null args)
                  0)
                 (t
                  (+ (first args)
                     (add (rest args)))))))
  (:expr moonli-expression
   :moonli "defun foo(&optional, a = 5): a end"
   :lisp (defun foo (&optional (a 5)) a)))


(define-moonli-macro lambda
  ((lambda-list lambda-parameter-list)
   (_ *whitespace/internal)
   (_ mandatory-colon)
   (_ *whitespace/internal)
   (body (esrap:? moonli)))
  `(lambda ,lambda-list ,@(rest body)))

(def-test lambda (macro-call)
  (:lisp (lambda () nil)
   :moonli "lambda (): nil end")
  (:lisp (lambda (x) x)
   :moonli "lambda (x):
  x
end")
  (:lisp (lambda (x y)
           (let ((sum (+ x y)))
             (expt sum 2)))
   :moonli "lambda (x, y):
  let sum = x + y:
    sum ^ 2
  end
end"))


(esrap:defrule defmethod/name
    (or (and expr:symbol
             *whitespace/internal
             expr:symbol)
        expr:symbol)
  (:function (lambda (args)
               (if (consp args)
                   (list (first args) (third args))
                   (list args)))))

(esrap:defrule defmethod/ll-parameter
    (or (and expr:symbol
             *whitespace/internal
             "::"
             *whitespace/internal
             (or expr:symbol expr:list))
        expr:symbol)
  (:function (lambda (args)
               (if (consp args)
                   (list (first args) (fifth args))
                   args))))

(esrap:defrule defmethod/lambda-list
    (or (and #\( *whitespace #\))
        (and #\(
             *whitespace
             defmethod/ll-parameter
             *whitespace
             (* (and #\, *whitespace defmethod/ll-parameter *whitespace))
             #\))
        (and #\(
             (+ (and *whitespace
                     defmethod/ll-parameter
                     *whitespace #\, *whitespace))
             #\)))
  (:function (lambda (expr)
               (if (null (cdddr expr))  ; length = 3, first or last
                   (mapcar #'second (second expr))
                   (cons (third expr)   ; middle
                         (mapcar #'third (fifth expr)))))))

(define-moonli-macro defmethod
  ((qualifier-name defmethod/name)
   (_ *whitespace/internal)
   (lambda-list defmethod/lambda-list)
   (_ *whitespace/internal)
   (_ #\:)
   (_ *whitespace/all)
   (body (esrap:? moonli)))
  `(defmethod ,@qualifier-name ,lambda-list
     ,@(rest body)))

(def-test defmethod (macro-call)
  (:moonli "defmethod our-identity(x): x end"
   :lisp (defmethod our-identity (x) x))
  (:moonli "defmethod :before our-identity(x):
  format(t, \"Returning identity~%\")
end"
   :lisp (defmethod :before our-identity (x)
           (format t "Returning identity~%")))
  (:moonli "defmethod :after our-identity(x):
  format(t, \"Returned identity~%\")
end"
   :lisp (defmethod :after our-identity (x)
           (format t "Returned identity~%")))
  (:moonli "defmethod add (x :: number, y :: number):
 x + y
end"
   :lisp (defmethod add ((x number) (y number))
           (+ x y)))
  (:moonli "defmethod add (x :: number, y :: number, &rest, others):
  x + if null(others):
    y
  else:
    apply(function(add), y, others)
  end
end"
   :lisp (defmethod add ((x number) (y number) &rest others)
           (+ x
              (cond ((null others)
                     y)
                    (t
                     (apply #'add y others))))))
  (:moonli "defmethod add (x :: number, y :: number, &rest, others):
  x + (if null(others):
    y
  else:
    apply(function(add), y, others)
  end)
end"
   :lisp (defmethod add ((x number) (y number) &rest others)
           (+ x
              (cond ((null others)
                     y)
                    (t
                     (apply #'add y others))))))
  (:moonli "defmethod add (x :: string, y):
  uiop:strcat(x, y)
end"
   :lisp (defmethod add ((x string) y)
           (uiop:strcat x y))))
