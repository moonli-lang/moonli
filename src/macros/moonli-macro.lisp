(in-package :moonli)

(5am:in-suite :moonli)

(esrap:defrule let-binding
    (and expr:symbol
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
    (and let-binding
         (* (and ","
                 +whitespace
                 let-binding
                 *whitespace)))
  (:function (lambda (expr)
               (if (null expr)
                   nil
                   (cons (first expr)
                         (mapcar #'third (second expr)))))))

(define-moonli-macro let

  ((let-bindings let-bindings)
   (_ *whitespace/internal)
   (_ ":")
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
  ((condition-then (and moonli-expression
                        *whitespace/internal
                        mandatory-colon
                        *whitespace
                        moonli))
   (_ *whitespace)
   (elif-clauses (* elif-clause))
   (else-part (esrap:? (and *whitespace
                            "else"
                            *whitespace/internal
                            mandatory-colon
                            *whitespace
                            moonli))))
  (let+ (((condition &ign &ign &ign then-part) condition-then)
         ((&optional &ign &ign &ign &ign &ign else-part) else-part))
    `(cond (,condition
            ,@(rest then-part))
           ,@elif-clauses
           (t
            ,@(rest else-part)))))


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

(define-moonli-macro deftype
  ((name expr:symbol)
   (_ *whitespace/internal)
   (lambda-list lambda-parameter-list)
   (_ *whitespace/internal)
   (_ ":")
   (_ *whitespace/all)
   (body (esrap:? moonli)))
  `(deftype ,name ,lambda-list
     ,@(rest body)))

(define-moonli-macro progn
  ((body (esrap:? moonli)))
  body)
