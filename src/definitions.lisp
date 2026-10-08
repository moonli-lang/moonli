(in-package :moonli)

(defun offset-from-file-and-form-number/lisp (form-number file-name)
  (with-open-file (f file-name :direction :input)
    (let ((*read-suppress* t))
      ;; Skip some forms
      (loop :repeat form-number
            :for form := (read f nil nil nil))
      ;; Go to the next non-whitespace and non-comment character
      (loop :for next-char := (peek-char t f nil nil nil)
            :do (cond ((member next-char '(#\newline #\return #\tab #\space))
                       (read-char f nil nil nil))
                      ((char= #\; next-char)
                       (read-line f nil nil nil))
                      (t
                       (return-from offset-from-file-and-form-number/lisp
                         (1+ (file-position f)))))))))

;; Default: Return nil
(defmethod offset-from-file-and-form-number (form-number file-name type)
  nil)

(defmethod offset-from-file-and-form-number (form-number file-name (type (eql :lisp)))
  (offset-from-file-and-form-number/lisp form-number file-name))

(defmethod offset-from-file-and-form-number (form-number file-name (type (eql :asd)))
  (offset-from-file-and-form-number/lisp form-number file-name))

(defmethod offset-from-file-and-form-number (form-number file-name (type (eql :lsp)))
  (offset-from-file-and-form-number/lisp form-number file-name))

(defun find-definitions-from-symbol (name)
  (remove nil
          (mapcar (lambda (def)
                    (destructuring-bind (&key file form offset)
                        (definitions:source-location def)
                      (when file
                        (list (list (definitions:type def)
                                    (definitions:symbol def))
                              (swank/backend:make-location
                               (list :file (namestring
                                            (if (typep file 'logical-pathname)
                                                (translate-logical-pathname file)
                                                file)))
                               (list :position (or (and form
                                                        (offset-from-file-and-form-number
                                                         form
                                                         file
                                                         (intern (string-upcase (pathname-type file))
                                                                 :keyword)))
                                                   offset
                                                   0))
                               (list :snippet ""))))))
                  (definitions:find-definitions name))))

;; Based exactly on swank:find-definition-for-emacs
(defun find-definitions-for-emacs (name)
  "Return a list ((DSPEC LOCATION) ...) of definitions for NAME.
DSPEC is a string and LOCATION a source location. NAME is a string."
  (declare (type string name))
  (multiple-value-bind (symbol found)
      (swank::find-definitions-find-symbol-or-package name)
    (when found
      (mapcar #'swank::xref>elisp
              (remove-duplicates (find-definitions-from-symbol symbol)
                                 :test #'equal)))))
