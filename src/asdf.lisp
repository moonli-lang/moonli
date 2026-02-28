;;; This class is defined in the ASDF package, so that the keyword :moonli works.
;;; See:  https://github.com/fare/asdf/blob/master/doc/best_practices.md#using-asdf-extensions

(in-package :asdf)

(defclass moonli-file (cl-source-file)
  ((type :initform "moonli")))

(defmethod perform ((o compile-op) (c moonli-file))
  (let ((moonli-file (first (input-files o c)))
        (fasl-file   (first (output-files o c))))
    (moonli:compile-moonli-file moonli-file fasl-file)))

(defmethod perform ((o load-op) (c moonli-file))
  (let* ((moonli-file (first (input-files o c)))
         (fasl-file   (first (output-files o c)))
         (debug-file (make-pathname :defaults moonli-file
                                    :type (uiop:strcat "debug." (pathname-type moonli-file)))))
    (if (string-equal "fasl" (pathname-type moonli-file))
        (progn
          (load moonli-file)
          (load debug-file))
        (moonli:load-moonli-file moonli-file :transpile nil))))
