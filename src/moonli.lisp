(in-package :moonli)

(defun read-moonli-from-stream (stream read-until-eof)
  (loop :with initial-input := (make-array 0 :element-type 'character
                                             :fill-pointer t)
        :with results := nil
        :with empty-line := nil
        :while (and (null results)
                    (null empty-line))
        :do (handler-case
                (handler-bind (((or moonli-parse-error
                                    esrap:esrap-parse-error)
                                 (lambda (c)
                                   (when empty-line
                                     (format *error-output* "~A~%" c)
                                     (uiop:print-backtrace :condition c)))))
                  (setf initial-input
                        (or (with-output-to-string (s initial-input)
                              (if read-until-eof
                                  (loop :while (listen stream)
                                        :do (write-char (read-char stream) s))
                                  (loop :for line := (read-line stream nil :end-of-file)
                                        :if (and (stringp line)
                                                 (zerop (length line)))
                                          :do (setf empty-line t)
                                              (return)
                                        :else
                                          :do (if (eq :end-of-file line)
                                                  (uiop:quit)
                                                  (write-line line s)))))
                            initial-input))
                  (setf results
                        (nconc results
                               (rest (read-moonli-from-string initial-input)))))
              ((or moonli-parse-error
                   esrap:esrap-parse-error)
                  nil))
        :finally (return `(progn ,@results))))

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
                                 (when (find-restart 'continue c)
                                   (invoke-restart 'continue c)))))
                (multiple-value-bind (result next-pos success)
                    (alexandria:ignore-some-conditions
                        (moonli-parse-error)
                      (let ((*transpilation-character-offset* pos))
                        (esrap:parse 'moonli-expression string
                                     :start pos :junk-allowed t)))
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
    (with-output-to-string (*standard-output*)
      (dolist (form (rest lisp-expr))
        (write form :case :downcase)
        (terpri)))))

(defun load-moonli-file (moonli-file &key (transpile t))
  (when (pathname-type moonli-file)
    (assert (string= "moonli" (pathname-type moonli-file))))
  (if transpile
      (multiple-value-bind (lisp-file debug-file)
          (transpile-moonli-file moonli-file)
        (load lisp-file)
        (load debug-file))
      (let ((file-contents (alexandria:read-file-into-string moonli-file))
            (file-position 0))
        (loop :while (and file-position
                          (< file-position (length file-contents)))
              :do (multiple-value-bind (expr pos success)
                      (esrap:parse `(or #\;
                                        whitespace
                                        moonli-expression)
                                   file-contents
                                   :junk-allowed t
                                   :start file-position)
                    (if success
                        (progn
                          (unless (typep expr 'comment-cst)
                            (eval expr))
                          (setf file-position pos))
                        (esrap:parse `(or #\;
                                          whitespace
                                          moonli-expression)
                                     file-contents
                                     :start file-position)))))))

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
         (debug-file (make-pathname :defaults moonli-file :type "debug.lisp"))
         (debug-loc  nil)
         (*transpilation-line-number* 0)
         (*transpilation-character-offset* 0)
         (*transpilation-definition-source-form-table* (make-hash-table :test #'equal))
         (target (read-moonli-from-string source)))
    (format *standard-output* ";  to ~A~%" (namestring target-file))
    ;; (setq *file-string* source)
    (with-open-file (out target-file
                         :if-does-not-exist :create
                         :if-exists :supersede
                         :direction :output)
      (with-open-file (debug debug-file
                             :if-does-not-exist :create
                             :if-exists :supersede
                             :direction :output)
        (let ((*package* (find-package :moonli)))
          (write `(in-package :moonli) :stream debug :case :downcase)
          (terpri debug))
        (with-standard-io-syntax
          (let ((*print-pretty* t))
            (format out ";;; This file was automatically generated.~%")
            (format out ";;; Do NOT edit by hand. It will be overwritten.~%")
            (format out ";;; Edit or Replace the corrsponding .moonli file instead!~%~%")
            (dolist (form (cdr target))
              (write form :stream out :case :downcase)
              (unless (comment-p form) (terpri out))
              (when (gethash form *transpilation-definition-source-form-table*)
                (push (third (gethash form *transpilation-definition-source-form-table*))
                      debug-loc))
              (unless (comment-p form) (format out "~%"))
              (may-be-eval-form form))))
        (with-standard-io-syntax
          (let* ((*print-pretty* t)
                 (*package* (find-package :moonli)))
            (loop :for form :in (cdr target)
                  :for debug-info := (gethash form *transpilation-definition-source-form-table*)
                  :if debug-info
                    :do (destructuring-bind (name line char) debug-info
                          (declare (ignore line))
                          (write `(update-defun-source-location ',name ,char)
                                 :stream debug
                                 :case :downcase)
                          (terpri debug)))))
        (format *standard-output* "; wrote ~A~%" (namestring target-file))))
    (values target-file debug-file)))

(defmethod definitions/swank:offset-from-file-and-form-number
    (form-number file-name (type (eql :moonli)))
  (with-open-file (f file-name :direction :input)
    (let ((*read-eval* nil))
      ;; Skip some forms
      (loop :with i := 0
            :while (< i form-number)
            :do (multiple-value-bind (form success)
                    (handler-case
                        (values (read-moonli-from-stream f nil) t)
                      ((or esrap:esrap-parse-error
                           moonli-parse-error)
                          ()
                        (values nil nil)))
                  (when (and success
                             (not (comment-p form)))
                    (incf i))))
      ;; Go to the next non-whitespace and non-comment character
      (loop :for next-char := (peek-char t f nil nil nil)
            :do (cond ((member next-char '(#\newline #\return #\tab #\space))
                       (read-char f nil nil nil))
                      ((char= #\# next-char)
                       (read-line f nil nil nil))
                      (t
                       (return-from definitions/swank:offset-from-file-and-form-number
                         (1+ (file-position f)))))))))

(defun compile-moonli-file (source-file fasl-file)
  (multiple-value-bind (lisp-source-file debug-file)
      (transpile-moonli-file source-file)
    (asdf:compile-file* lisp-source-file :output-file fasl-file)
    (asdf:compile-file* debug-file
                        :output-file (make-pathname :defaults fasl-file
                                                    :type (uiop:strcat "debug." (pathname-type fasl-file))))))

#|
1. We want an extensible system to recognize moonli macros such as "LET". ; ;
2. This neats to interface with the file reading above. ; ;
|#
