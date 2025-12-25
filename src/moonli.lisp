(in-package :moonli)

(defun read-moonli-from-stream (stream read-until-eof)
  (read-moonli-from-string
   (with-output-to-string (*standard-output*)
     (if read-until-eof
         (loop :while (listen stream)
               :do (write-char (read-char stream)))
         (loop :for line := (read-line stream nil :end-of-file)
               :if (and (stringp line)
                        (zerop (length line)))
                 :do (return)
               :else
                 :do (if (eq :end-of-file line)
                         (uiop:quit)
                         (write-line line)))))))

(defun read-moonli-from-string (string &optional read-partial)
  "NOTE: Some moonli forms like defpackage and in-package can have side-effects.

  If READ-PARTIAL is true, no error is raised, but the STRING is read until
prior to transpilation errors.

  Returns three values:

- lisp expression
- the position in string until which read succeeded
- whether the entirety of STRING was read
"
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
          :with may-be-errors := nil
          :while (< pos end)
          :do (handler-bind ((moonli-may-be-parse-error
                               (lambda (c)
                                 (push c may-be-errors)
                                 (invoke-restart 'continue c))))
                (multiple-value-bind (result next-pos success)
                    (alexandria:ignore-some-conditions
                        (moonli-parse-error)
                      (esrap:parse 'moonli-expression string
                                   :start pos :junk-allowed t))
                  (cond ((and (not success)
                              (not read-partial))
                         (if (zerop pos)
                             (esrap:parse 'moonli-expression string
                                          :start pos)
                             (error 'moonli-parse-error
                                    :position pos
                                    :may-be-errors may-be-errors)))
                        ((and (not success)
                              read-partial)
                         (return nil))
                        (success
                         (push result exprs)
                         (setf pos (or next-pos end))
                         (multiple-value-bind (ws new-pos)
                             (esrap:parse '*whitespace/all string
                                          :start pos :junk-allowed t)
                           (dolist (ws_ ws)
                             (when ws_ (push ws_ exprs)))
                           (setf pos (or new-pos end))))))))
    (values `(progn ,@(nreverse exprs))
            pos
            (= pos (length string)))))

(defun moonli-string-to-lisp-string (string)
  (let ((lisp-expr (read-moonli-from-string string t)))
    (if lisp-expr
        (with-output-to-string (*standard-output*)
          (dolist (form (rest lisp-expr))
            (write form :case :downcase)
            (terpri)))
        string)))

(defun load-moonli-file (moonli-file &key (transpile t))
  (assert (string= "moonli" (pathname-type moonli-file)))
  (if transpile
      (load (transpile-moonli-file moonli-file))
      (eval (read-moonli-from-string
             (alexandria:read-file-into-string moonli-file)))))

(defun may-be-eval-form (form)
  (let ((expanded (swank/backend:macroexpand-all form)))
    (if (and (consp expanded)
             (eq 'cl:eval-when (first expanded))
             (or (member :compile-toplevel (second expanded))
                 (member :load-toplevel (second expanded))))
        (eval form)
        form)))

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
      (with-standard-io-syntax
        (let ((*print-pretty* t))
          (format out ";;; This file was automatically generated.~%")
          (format out ";;; Do NOT edit by hand. It will be overwritten.~%")
          (format out ";;; Edit or Replace the corrsponding .moonli file instead!~%~%")
          (dolist (form (cdr target))
            (write form :stream out :case :downcase)
            (format out "~%~%")
            (may-be-eval-form form)))
        (format *standard-output* "; wrote ~A~%" (namestring target-file))))
    target-file))

(defun compile-moonli-file (source-file fasl-file)
  (let ((lisp-source-file (transpile-moonli-file source-file)))
    (asdf:compile-file* lisp-source-file :output-file fasl-file)))

#|
1. We want an extensible system to recognize moonli macros such as "LET". ; ;
2. This neats to interface with the file reading above. ; ;
|#
