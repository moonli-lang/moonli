(in-package :moonli)

(5am:in-suite :moonli)

(esrap:defrule defstruct/slot
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
             (and expr:symbol
                  +whitespace/internal
                  "="
                  +whitespace/internal
                  moonli-expression)
             (and expr:symbol))
         #\;
         *whitespace)
  (:function (lambda (expr)
               (cond ((= 5 (length (first expr)))
                      (list (first (first expr))
                            (fifth (first expr))))
                     (t
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
                                           (nth 6 third)))))))))))

(define-moonli-macro defstruct
  ((name expr:symbol)
   (_ *whitespace/internal)
   (_ ":")
   (_ *whitespace/all)
   (slots (* defstruct/slot)))
  `(defstruct ,name ,@slots))

(def-test defstruct (macro-call)
  (:moonli "defstruct foo:
  a;
  b;
end"
   :lisp (defstruct foo a b))
  (:moonli "defstruct foo:
  (a = 4) :: number;
  b;
end"
   :lisp (defstruct foo (a 4 :type number) b))
  (:moonli "defstruct foo:
  (a = 4), :read-only = t;
  b;
end"
   :lisp (defstruct foo (a 4 :read-only t) b))
  (:moonli "defstruct foo:
  (a = 4), :read-only = t;
  (b = 2.0) :: single-float, :read-only = t;
end"
   :lisp (defstruct foo
           (a 4 :read-only t)
           (b 2.0 :type single-float :read-only t)))
  (:moonli "defstruct foo:
  a = 4;
  b = 2.0;
end"
   :lisp (defstruct foo
           (a 4)
           (b 2.0))))
