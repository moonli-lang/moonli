(in-package :moonli)

(esrap:defrule let+-binding
    (and (or chain
             good-symbol
             bracketed-expression
             expr:list)
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


(esrap:defrule let+-bindings
    (or (and let+-binding
             (* (and #\,
                     +whitespace
                     let+-binding
                     *whitespace)))
        (* whitespace))
  (:function (lambda (expr)
               (if (null expr)
                   nil
                   (cons (first expr)
                         (mapcar #'third (second expr)))))))


(define-moonli-macro let-plus:let+
  ((let-bindings let+-bindings)
   (_ *whitespace)
   (_ mandatory-colon)
   (let-body (esrap:? moonli)))
  `(let-plus:let+ ,let-bindings
     ,@(rest let-body)))

(def-test let-plus:let+ (macro-call)
  (:lisp (let-plus:let+ ((x 42)) x)
   :moonli "let-plus:let+ x = 42: x
end")
  (:lisp (let-plus:let+ (((a b) (list 1 2)))
           (+ a b))
   :moonli "let-plus:let+ (a,b) = list(1,2):
  a + b
end")
  (:lisp (let-plus:let+ (((let-plus:&values a b) (list 1 2)))
           (+ a b))
   :moonli "let-plus:let+ let-plus:&values(a,b) = list(1,2):
  a + b
end")
  (:lisp (let-plus:let+ (((let-plus:&values a b) (list 1 2))
                         ((c d e) (list 1 2 3)))
           (fill-hash-set a b c d e))
   :moonli "let-plus:let+
  let-plus:&values(a,b) = list(1,2),
  (c,d,e) = list(1,2,3):
  {a,b,c,d,e}
end"))

