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
