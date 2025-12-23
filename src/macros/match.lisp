(in-package :moonli)

(5am:in-suite :moonli)

(esrap:defrule match/clauses
    (* (and *whitespace
            chain
            *whitespace/internal
            ":"
            *whitespace
            moonli-expression
            ";"))
  (:function (lambda (expr)
               (mapcar (lambda (expr)
                         (list (second expr)
                               (sixth expr)))
                       expr))))

(define-moonli-macro optima:match
  ((arg moonli-expression)
   (_ (and *whitespace/internal ":" *whitespace))
   (clauses match/clauses))
  `(optima:match ,arg
     ,@clauses))

(def-test optima:match (macro-call)
  (:moonli "optima:match 5:
  list(_) : 42;
  sym : sym;
end"
   :lisp (optima:match 5
           ((list _) 42)
           (sym sym)))
  (:moonli "optima:match (2,3,4):
  list(x, y, z) : x + y + z;
  sym : sym;
end"
   :lisp (optima:match (list 2 3 4)
           ((list x y z)
            (+ (+ x y) z))
           (sym sym)))
  (:moonli "optima:match [2,3,4]:
  vector(x, y, z) : x + y + z;
  sym : sym;
end"
   :lisp (optima:match (vector 2 3 4)
           ((vector x y z)
            (+ (+ x y) z))
           (sym sym)))
  (:moonli "optima:match [2,3,(42,\"hello world\")]:
  vector(x, y, list(num, _)) : x + y + num;
  sym : sym;
end"
   :lisp (optima:match (vector 2 3 (list 42 "hello world"))
           ((vector x y (list num _))
            (+ (+ x y) num))
           (sym sym))))
