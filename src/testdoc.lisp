(defpackage :moonli/macro-tests
  (:use))

(in-package :moonli)

(defun name-test-symbol (name)
  (intern (format nil "~A::~A"
                  (package-name (symbol-package name))
                  (symbol-name name))
          '#:moonli/macro-tests))

(defvar *moonli-macro-snippets* (make-hash-table :test #'eq)
  "Maps moonli macro names to example snippets defined in DEF-TEST forms.")

(defun moonli-macro-snippet (name format)
  (ecase format
    (:md
     (with-output-to-string (*standard-output*)
       (format t "```moonli")
       (loop :for snippet :in (gethash name *moonli-macro-snippets*)
             :do (format t "~%~A~%" snippet))
       (format t "```")))
    (:org
     (with-output-to-string (*standard-output*)
       (format t "#+begin_src moonli")
       (loop :for snippet :in (gethash name *moonli-macro-snippets*)
             :do (format t "~%~A~%" snippet))
       (format t "#+end_src")))))

(defvar *moonli-macro-transpilation-snippets* (make-hash-table :test #'eq)
  "Maps moonli macro names to example snippet transpilations defined in DEF-TEST forms.")

(defun moonli-macro-transpilation-snippet (name format)
  (with-standard-io-syntax
    (let ((*print-pretty* t)
          (*package* (find-package :moonli)))
      (ecase format
        (:md
         (with-output-to-string (*standard-output*)
           (loop :for (moonli . lisp)
                   :in (gethash name *moonli-macro-transpilation-snippets*)
                 :do (format t "~%```moonli~%~a~%```~%" moonli)
                     (format t "~%transpiles to~%")
                     (let ((*print-case* :downcase))
                       (format t "~%```common-lisp~%~s~%```~%" lisp)))))
        (:org
         (with-output-to-string (*standard-output*)
           (loop :for (moonli . lisp)
                   :in (gethash name *moonli-macro-transpilation-snippets*)
                 :do (format t "~%#+begin_src moonli~%~a~%#+end_src~%" moonli)
                     (format t "~%transpiles to~%")
                     (let ((*print-case* :downcase))
                       (format t "~%#+begin_src common-lisp~%~s~%#+end_src~%" lisp)))))))))

(defmacro def-test (name (default-expr) &body target-transpilations)
  "Each element of TARGET-TRANSPILATIONS must be a plist with keys LISP and MOONLI. Optionally, it may have an entry for EXPR that overrides DEFAULT-EXPR"
  (let* ((test-name (name-test-symbol name)))
    (flet ((def-form (&key lisp moonli expr)
             `(5am:is (equal ',lisp
                             (esrap:parse ',(or expr default-expr) ,moonli)))))
      `(progn
         (5am:def-test ,test-name ()
           ,@(mapcar (lambda (target)
                       (apply #'def-form target))
                     target-transpilations))
         (setf (gethash ',name *moonli-macro-transpilation-snippets*)
               ',(mapcar (lambda (target)
                           (cons (getf target :moonli)
                                 (getf target :lisp)))
                         target-transpilations))
         (setf (gethash ',name *moonli-macro-snippets*)
               ',(mapcar (lambda (target)
                           (getf target :moonli))
                         target-transpilations))))))
