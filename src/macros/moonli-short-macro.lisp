(in-package :moonli)

(5am:in-suite :moonli)

(define-moonli-short-macro defvar
  ((name good-symbol)
   (_ +whitespace/internal)
   (_ "=")
   (_ +whitespace/internal)
   (value moonli-expression))
  `(defvar ,name ,value))

(def-test defvar (short-macro-call)
  (:lisp (defvar a 5)
   :moonli "defvar a = 5"))

(define-moonli-short-macro defparameter
  ((name good-symbol)
   (_ +whitespace/internal)
   (_ "=")
   (_ +whitespace/internal)
   (value moonli-expression))
  `(defparameter ,name ,value))

(def-test defparameter (short-macro-call)
  (:lisp (defparameter a 5)
   :moonli "defparameter a = 5"))

(define-moonli-short-macro in-package
  ((name expr:symbol))
  (setf *package* (find-package name))
  `(in-package ,name))

(5am:def-test moonli/macro-tests::|common-lisp::in-package| ()
  (unwind-protect
       (progn
         (make-package "MOONLI/TEST/IN-PACKAGE")
         (5am:is (eq (find-package "MOONLI/TEST/IN-PACKAGE")
                     (symbol-package
                      (alexandria:lastcar
                       (read-moonli-from-string
                        "in-package moonli/test/in-package; foo"))))))
    (delete-package "MOONLI/TEST/IN-PACKAGE")))

(define-moonli-short-macro declare
  ((decl-specs (and chain
                    (* (and *whitespace/internal
                            #\,
                            *whitespace/internal
                            chain)))))
  `(declare ,(first decl-specs)
            ,@(mapcar #'fourth (second decl-specs))))

(def-test declare (short-macro-call)
  (:lisp (declare (type single-float x y))
   :moonli "declare type(single-float, x, y)")
  (:lisp (declare (type single-float x y)
                  (optimize (debug 3)))
   :moonli "declare type(single-float, x, y), optimize(debug(3))"))

(define-moonli-short-macro declaim
  ((decl-specs (and chain
                    (* (and *whitespace/internal
                            #\,
                            *whitespace/internal
                            chain)))))
  `(declaim ,(first decl-specs)
            ,@(mapcar #'fourth (second decl-specs))))

(def-test declaim (short-macro-call)
  (:lisp (declaim (inline foo))
   :moonli "declaim inline(foo)")
  (:lisp (declaim (type hash-table *map*))
   :moonli "declaim type(hash-table, *map*)"))

(define-moonli-short-macro defgeneric
  ((fun-name expr:symbol)
   (_ *whitespace/internal)
   (lambda-list lambda-parameter-list))
  `(defgeneric ,fun-name ,lambda-list))

(def-test defgeneric (short-macro-call)
  (:lisp (defgeneric area (shape))
   :moonli "defgeneric area(shape)"))
