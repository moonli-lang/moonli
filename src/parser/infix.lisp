(in-package :moonli)

(5am:in-suite :moonli)

;;; In terms of useability, it seems a bad idea to allow symbols from
;;; different packages to have different precedence despite having
;;; same names. Thus, we will not make infix operators depend on symbols.
;;; They will only directly relate to strings.

(defun infix-operator-to-symbol (op-string)
  (alexandria:switch (op-string :test #'string=)
    ("=" (find-symbol "SETF"))
    ("==" (find-symbol "="))
    ("!=" (find-symbol "/="))
    ("^" (find-symbol "EXPT"))
    (t (find-symbol (string-upcase (string op-string))))))

(defun process-binary-infix-expression (expr)
  `(,(infix-operator-to-symbol (third expr))
    ,(first expr)
    ,(fifth expr)))

(defun process-nary-infix-expression (expr)
  (labels ((build-prefix (expr)
           (if (null (second expr))
               (first expr)
               (destructuring-bind (first (first-rest &rest rest-rest))
                   expr
                 (build-prefix
                  (list `(,(infix-operator-to-symbol (second first-rest))
                          ,first
                          ,(fourth first-rest))
                        rest-rest))))))
    (build-prefix expr)))

(esrap:defrule infix-expression
    (or assignment
        disjunction)
  (:error-report nil))

(esrap:defrule assignment
    (and chain
         +whitespace/all
         "="
         +whitespace/all
         infix-expression)
  (:function process-binary-infix-expression))

(esrap:defrule disjunction
    (and conjunction
         (* (and +whitespace/all
                 "or"
                 +whitespace/all
                 conjunction)))
  (:function process-nary-infix-expression))

(esrap:defrule conjunction
    (and optional-negation
         (* (and +whitespace/all
                 "and"
                 +whitespace/all
                 optional-negation)))
  (:function process-nary-infix-expression))

(esrap:defrule optional-negation
    (or negation comparison))

(esrap:defrule negation
    (and "not" +whitespace/internal math-expression)
  (:function (lambda (expr)
               `(,(find-symbol "NOT") ,(third expr)))))

(esrap:defrule comparison
    (and math-expression
         (* (and +whitespace/all
                 (or "<=" "<" "==" "!=" ">=" ">")
                 +whitespace/all
                 math-expression)))
  (:function process-nary-infix-expression))

(esrap:defrule math-expression
    sum)

(esrap:defrule sum
    (and product
         (* (and +whitespace/all
                 (or #\+ #\-)
                 +whitespace/all
                 product)))
  (:function process-nary-infix-expression))

(esrap:defrule product
    (and optional-expt
         (* (and +whitespace/all
                 (or #\* #\/)
                 +whitespace/all
                 optional-expt)))
  (:function process-nary-infix-expression))

(esrap:defrule optional-expt
    (or expt optional-unary-minus))

(esrap:defrule expt
    (and optional-unary-minus
         +whitespace/all
         #\^
         +whitespace/all
         optional-unary-minus)
  (:function process-binary-infix-expression))

(esrap:defrule optional-unary-minus
    (or unary-minus optionally-typed-expression))

(esrap:defrule unary-minus
    (and "-" +whitespace/internal optionally-typed-expression)
  (:function (lambda (expr)
               `(,(find-symbol "-") ,(third expr)))))

(esrap:defrule optionally-typed-expression
    (or typed-expression
        macro-call
        short-macro-call
        atomic-expression))

(esrap:defrule typed-expression
    (and atomic-expression
         *whitespace/internal
         "::"
         *whitespace/internal
         atomic-expression)
  (:function (lambda (expr)
               `(the ,(fifth expr)
                     ,(first expr)))))

(5am:def-test infix-expression ()
  (5am:is (equal `(+ 2 (cond ((zerop x)
                              -1)
                             (t
                              1)))
                 (esrap:parse 'infix-expression "2 + if zerop(x): -1 else: 1 end")))
  (5am:is (equal `(+ (* 2 3) 4)
                 (esrap:parse 'infix-expression "2 * 3 + 4")))
  (5am:is (equal `(- 3)
                 (esrap:parse 'infix-expression "- 3")))
  (5am:is (equal `(+ 2 (* 3 4))
                 (esrap:parse 'infix-expression "2 + 3 * 4")))
  (5am:is (equal `(setf (aref a 0 1)
                        (or (aref b 0 1)
                            (aref c 0 1)))
                 (esrap:parse 'infix-expression
                              "aref(a,0,1) = aref(b,0,1) or aref(c,0,1)")))
  (5am:is (equal `(setf (aref a 0)
                        (+ (+ (aref a 1)
                              (aref a 2))
                           (aref a 3)))
                 (esrap:parse 'infix-expression
                              "aref(a,0) = aref(a,1) + aref(a,2) + aref(a,3)")))
  (5am:is (equal `(- (+ (* 2 3)
                        (* (/ 4 5) 3))
                     (expt 2 10))
                 (esrap:parse 'infix-expression "2 * 3 + 4 / 5 * 3 - 2 ^ 10")))
  (5am:is (equal `(- (+ (* (- 2) 3)
                        (* (/ 4 5) 3))
                     (expt 2 10))
                 (esrap:parse 'infix-expression "- 2 * 3 + 4 / 5 * 3 - 2 ^ 10"))))

(5am:def-test typed-expression ()
  (5am:is (equal `(the number 2)
                 (esrap:parse 'moonli-expression "2::number")))
  (5am:is (equal `(the number 2)
                 (esrap:parse 'moonli-expression "(2::number)")))
  (5am:is (equal `(the number a)
                 (esrap:parse 'moonli-expression "a::number")))
  (5am:is (equal `(the number a)
                 (esrap:parse 'moonli-expression "(a::number)")))
  (5am:is (equal `(+ 2 (the number a))
                 (esrap:parse 'moonli-expression "2 + (a::number)")))
  (5am:is (equal `(the number (+ 2 a))
                 (esrap:parse 'moonli-expression "(2 + a)::number"))))
