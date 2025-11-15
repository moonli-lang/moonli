(in-package :moonli)

(5am:in-suite :moonli)

(esrap:defrule let-binding
    (and good-symbol
         +whitespace
         #\=
         +whitespace
         moonli-expression)
  (:function (lambda (expr)
               (list (let ((var-form (first expr)))
                       (if (and (listp var-form)
                                (eq 'list (first var-form)))
                           (rest var-form)
                           var-form))
                     (fifth expr)))))

(esrap:defrule let-bindings
    (or (and let-binding
             (* (and mandatory-comma
                     +whitespace
                     let-binding
                     *whitespace)))
        (* whitespace))
  (:function (lambda (expr)
               (if (null expr)
                   nil
                   (cons (first expr)
                         (mapcar #'third (second expr)))))))

(define-moonli-macro let

  ((let-bindings let-bindings)
   (_ *whitespace/internal)
   (_ mandatory-colon)
   (let-body (esrap:? moonli)))

  `(let ,let-bindings
     ,@(rest let-body)))

(def-test let (macro-call)
  (:lisp (let ((a 2) (b 3))
           (+ a b))
   :moonli "let a = 2, b = 3:
   a + b
end")
  (:lisp (let ((a 2) (b 3))
           (+ a b))
   :moonli "let a = 2, b = 3:
   a + b
end let"))



(esrap:defrule elif-clause
    (and *whitespace
         "elif"
         *whitespace
         moonli-expression
         *whitespace/internal
         mandatory-colon
         moonli
         *whitespace)
  (:function (lambda (expr)
               (optima:ematch expr
                 ((list _ _ _ condition _ _ statements _)
                  `(,condition ,@(rest statements)))))))

(define-moonli-macro if
  ((condition moonli-expression)
   (_ (and *whitespace/internal mandatory-colon *whitespace))
   (then-part moonli)
   (_ *whitespace)
   (elif-clauses (* elif-clause))
   (_ (esrap:? (and *whitespace "else" *whitespace/internal mandatory-colon *whitespace)))
   (else-part (esrap:? moonli)))
  `(cond (,condition
          ,@(rest then-part))
         ,@elif-clauses
         (t
          ,@(rest else-part))))


(def-test if (macro-call)
  (:lisp (cond (a b) (t))
   :moonli "if a: b end if")
  (:lisp (cond (a b c) (t))
   :moonli "if a:
  b; c
end")
  (:lisp (cond (a b) (t c))
   :moonli "if a: b
else: c
end if")
  (:lisp (cond (a b d) (t c e))
   :moonli "if a:
   b; d
else:
   c; e
end if")
  (:lisp (cond (a b) (c d e) (t f))
   :moonli "if a: b
elif c: d; e
else: f
end if")
  (:lisp (the boolean (cond (a b) (t c)))
   :moonli "(if a: b else: c; end)::boolean"
   :expr moonli-expression)
  (:lisp (cond ((null args)
                0)
               (t
                1))
   :moonli "if null(args): 0; else: 1 end")
  (:lisp (cond ((null args)
                0)
               (t
                (first args)))
   :moonli "if null(args):
    0
else:
    first(args)
end if")
  (:lisp (cond ((null args)
                0)
               (t
                (+ 2 3)))
   :moonli "if null(args):
  0
else:
  2 + 3
end if")
  (:lisp (cond ((null args)
                0)
               (t
                (+ (first args)
                   (add (rest args)))))
   :moonli "if null(args):
  0
else:
  first(args) + add(rest(args))
end if"))



(5am:def-test macros-are-package-local ()
  (unwind-protect
       (handler-bind ((warning #'muffle-warning))
         (make-package "DUMMY")
         (intern "IF" "DUMMY")
         (export (find-symbol "IF" "DUMMY") "DUMMY")
         (eval `(define-moonli-macro ,(find-symbol "IF" "DUMMY")
                  ((test moonli-expression)
                   (_ +whitespace/internal)
                   (then moonli-expression)
                   (_ +whitespace/internal)
                   (else moonli-expression))
                  (list 'if test then else)))
         (let ((*package* (find-package "DUMMY")))
           (5am:is (equal `(if "hello" "world" "bye")
                          (esrap:parse 'macro-call "if \"hello\" \"world\" \"bye\" end"))))
         (let ((*package* (find-package :moonli)))
           (5am:is (equal `(cond ("hello" "world") (t "bye"))
                          (esrap:parse 'macro-call "if \"hello\": \"world\"; else: \"bye\" end")))
           (5am:is (equal `(if "hello" "world" "bye")
                          (esrap:parse 'macro-call "dummy:if \"hello\" \"world\" \"bye\" end")))))
    (if (find-package "DUMMY") (delete-package "DUMMY"))))

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


(esrap:defrule defpackage-option
    (and string-designator
         *whitespace
         (esrap:? (or (and string-designator
                           *whitespace
                           (* (and #\,
                                   *whitespace
                                   string-designator
                                   *whitespace)))
                      (+ (and *whitespace
                              string-designator
                              *whitespace
                              #\,
                              *whitespace))))
         #\;
         *whitespace)
  (:function (lambda (expr)
               (optima:ematch expr
                 ((list option-name _ args _ _)
                  `(,(intern (string-upcase option-name) :keyword)
                    ,@(if (null (nthcdr 3 args)) ; length=3, first option
                          (cons (first args)
                                (mapcar #'third (third args)))
                          (mapcar #'second args))))))))

(define-moonli-macro defpackage
  ((name string-designator)
   (_ *whitespace)
   (options (* defpackage-option)))
  (let ((form `(defpackage ,name ,@options)))
    (eval form)
    form))

(define-moonli-macro loop
  ((body (esrap:? moonli)))
  `(loop ,@(rest body)))

(def-test loop (macro-call)
  (:lisp (loop)
   :moonli "loop end loop")
  (:lisp (loop :repeat n :do (print "hello"))
   :moonli "loop :repeat n :do
  print(\"hello\")
end")
  (:lisp (loop :for i :below n :do (print (+ i 1)))
   :moonli "loop :for i :below n :do
  print(i + 1)
end"))

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



(esrap:defrule defstruct-slot
    (and (or (and (and #\(
                       *whitespace/internal
                       expr:symbol
                       +whitespace/internal
                       "="
                       +whitespace/internal
                       moonli-expression
                       *whitespace/internal
                       #\))
                  (esrap:? (and +whitespace/internal
                                "::"
                                +whitespace/internal
                                chain))
                  (esrap:? (and #\,
                                *whitespace/internal
                                expr:symbol
                                +whitespace/internal
                                "="
                                +whitespace/internal
                                chain)))
             (and expr:symbol))
         #\;
         *whitespace)
  (:function (lambda (expr)
               (destructuring-bind
                   (first &optional second third &rest ign) (first expr)
                 (declare (ignore ign))
                 (if (symbolp first)
                     first
                     (nconc (list (nth 2 first)
                                  (nth 6 first))
                            (unless (null second)
                              (list :type (nth 3 second)))
                            (unless (null third)
                              (list (nth 2 third)
                                    (nth 6 third)))))))))

(define-moonli-macro defstruct
  ((name expr:symbol)
   (_ *whitespace)
   (slots (* defstruct-slot)))
  `(defstruct ,name ,@slots))

(def-test defpackage (macro-call)
  (:moonli "defpackage foo
  :use cl;
end"
   :lisp (defpackage foo (:use cl))))

(def-test defstruct (macro-call)
  (:moonli "defstruct foo
  a;
  b;
end"
   :lisp (defstruct foo a b))
  (:moonli "defstruct foo
  (a = 4) :: number;
  b;
end"
   :lisp (defstruct foo (a 4 :type number) b))
  (:moonli "defstruct foo
  (a = 4), :read-only = t;
  b;
end"
   :lisp (defstruct foo (a 4 :read-only t) b))
  (:moonli "defstruct foo
  (a = 4), :read-only = t;
  (b = 2.0) :: single-float, :read-only = t;
end"
   :lisp (defstruct foo
           (a 4 :read-only t)
           (b 2.0 :type single-float :read-only t))))
