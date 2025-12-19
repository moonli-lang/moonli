(in-package :moonli)

(5am:in-suite :moonli)

(esrap:defrule labels/definition
    (and expr:symbol
         *whitespace
         lambda-parameter-list
         *whitespace/internal
         ":"
         *whitespace
         (esrap:? moonli)
         *whitespace
         "end")
  (:function (lambda (expr)
               (optima:match expr
                 ((list name _ lambda-list _ _ _ body _ _)
                  `(,name ,lambda-list ,@(rest body)))))))

(esrap:defrule labels/definitions
    (and labels/definition
         (* (and *whitespace/internal
                 ","
                 *whitespace/all
                 labels/definition)))
  (:function (lambda (expr)
               (cons (first expr)
                     (mapcar #'fourth (second expr))))))

(define-moonli-macro labels
  ((definitions (esrap:? labels/definitions))
   (_ *whitespace/internal)
   (_ ":")
   (_ *whitespace/all)
   (body (esrap:? moonli)))
  `(labels ,definitions ,@(rest body)))

(def-test labels (macro-call)
  (:lisp (labels ((foo (x)
                    (bar (- x 1)))
                  (bar (x)
                    (cond ((< x 0)
                           nil)
                          (t
                           (foo (- x 1))))))
           (foo 42))
   :moonli "labels foo(x):
         bar(x - 1)
       end,
       bar(x):
         if (x < 0): nil else: foo(x - 1) end
       end:
  foo(42)
end")
  (:lisp (labels ((foo (x)
                    (cond ((< x 0)
                           nil)
                          (t
                           (foo (- x 1))))))
           (foo 42))
   :moonli "labels foo(x):
         if (x < 0): nil else: foo(x - 1) end
       end:
  foo(42)
end")
  (:lisp (labels ()
           nil)
   :moonli "labels :
  nil
end"))
