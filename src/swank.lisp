(in-package :moonli)

(defvar *swank-modified-p* nil)

(defun swank-add-moonli-transpiler ()
  (unless *swank-modified-p*
    (setf (fdefinition 'original-swank-compile-string)
          (fdefinition 'swank/backend:swank-compile-string))
    (eval `(defun swank/backend:swank-compile-string
               (string &rest args
                &key buffer position filename
                  line column policy)
             (declare (ignorable buffer position filename
                                 line column policy))
             (when (and (stringp filename)
                        (uiop:string-suffix-p filename ".moonli"))
               (setf string (moonli-string-to-lisp-string string)))
             (apply #'original-swank-compile-string string args)))
    (setf *swank-modified-p* t)))


