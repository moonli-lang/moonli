(in-package :moonli)

(defun read-moonli-from-stream (stream)
  (read-moonli-from-string
   (with-output-to-string (*standard-output*)
     (loop :while (listen stream)
           :do (write-char (read-char stream))))))

(defun read-moonli-from-string (string)
  "NOTE: Some moonli forms like defpackage and in-package can have side-effects."
  (let ((end (length string))
        (pos 0)
        (exprs ())
        (*moonli-parse-string* string))
    (loop :initially (multiple-value-bind (ws new-pos)
                         (esrap:parse '*whitespace/all string
                                      :start pos :junk-allowed t)
                       (dolist (ws_ ws)
                         (when ws_ (push ws_ exprs)))
                       (setf pos (or new-pos end)))
          :while (< pos end)
          :do (multiple-value-bind (result next-pos success)
                  (esrap:parse 'moonli-expression string
                               :start pos :junk-allowed t)
                (unless success
                  (error 'moonli-parse-error :position pos))
                (push result exprs)
                (setf pos (or next-pos end))
                (multiple-value-bind (ws new-pos)
                    (esrap:parse '*whitespace/all string :start pos :junk-allowed t)
                  (dolist (ws_ ws)
                    (when ws_ (push ws_ exprs)))
                  (setf pos (or new-pos end)))))
    `(progn ,@(nreverse exprs))))

(defun moonli-string-to-lisp-string (string)
  (let ((lisp-expr (ignore-errors (read-moonli-from-string string))))
    (if lisp-expr
        (with-output-to-string (*standard-output*)
          (dolist (form (rest lisp-expr))
            (write form :case :downcase)))
        string)))

(defun load-moonli-file (moonli-file &key (transpile t))
  (assert (string= "moonli" (pathname-type moonli-file)))
  (if transpile
      (load (transpile-moonli-file moonli-file))
      (eval (read-moonli-from-string
             (alexandria:read-file-into-string moonli-file)))))

(defun transpile-moonli-file (moonli-file)
  (format *standard-output* "; transpiling ~A~%" (namestring moonli-file))
  (let* ((source (alexandria:read-file-into-string moonli-file))
         (target-file (make-pathname :defaults moonli-file :type "lisp"))
         (target (read-moonli-from-string source)))
    (format *standard-output* ";  to ~A~%" (namestring target-file))
    ;; (setq *file-string* source)
    (with-open-file (out target-file
                         :if-does-not-exist :create
                         :if-exists :supersede
                         :direction :output)
      (format out ";;; This file was automatically generated.~%")
      (format out ";;; Do NOT edit by hand. It will be overwritten.~%")
      (format out ";;; Edit or Replace the corrsponding .moonli file instead!~%~%")
      (dolist (form (cdr target))
        (write form :stream out :case :downcase)
        (terpri out)
        (terpri out)))
    (format *standard-output* "; wrote ~A~%" (namestring target-file))
    target-file))

(defun compile-moonli-file (source-file fasl-file)
  (let ((lisp-source-file (transpile-moonli-file source-file)))
    (asdf:compile-file* lisp-source-file :output-file fasl-file)))

#|
1. We want an extensible system to recognize moonli macros such as "LET". ; ;
2. This neats to interface with the file reading above. ; ;
|#
