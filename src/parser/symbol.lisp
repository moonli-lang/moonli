(in-package :moonli)

(defun string-invert-case (string)
  (declare (optimize speed))
  (let ((copy-text (copy-seq string)))
    (loop :for pos :below (length copy-text)
          :for char := (char copy-text pos)
          :do (setf (char copy-text pos)
                    (cond ((lower-case-p char)
                           (char-upcase char))
                          ((upper-case-p char)
                           (char-downcase char))
                          (t
                           char))))
    copy-text))

(esrap:defrule simple-symbol
    (or (and #\|
             (+ (or (and #\\ #\|)
                    (not #\|)))
             #\|)
        (+ (not non-symbol-chars)))
  (:text t)
  (:function string-invert-case))

(esrap:defrule expr:symbol
    (or (and #\: simple-symbol)
        (and simple-symbol #\: simple-symbol)
        (and simple-symbol))
  (:function (lambda (expr)
               (optima:match expr
                 ((list package-name ":" symbol-name)
                  (let ((package (find-package package-name)))
                    (if package
                        (intern symbol-name package)
                        (error (format nil "Package with name ~A does not exist while reading ~A:~A"
                                       package-name
                                       (string-invert-case package-name)
                                       (string-invert-case symbol-name))))))
                 ((list ":" symbol-name)
                  (intern symbol-name :keyword))
                 ((list symbol-name)
                  (intern symbol-name)))))
  (:error-report :context))

(defun good-symbol-p (symbol)
  ;; Excluding these symbols is necessary, otherwise parser
  ;; cannot tell whether this symbol appears as syntactic part of the
  ;; expression or semantic
  (not (or (member symbol '(end elif else)
                   :test #'string-equal)
           (gethash symbol *moonli-macro-functions*)
           (gethash symbol *moonli-short-macro-functions*)
           (ignore-errors
            (parse-number:parse-number (symbol-name symbol))))))

(esrap:defrule good-symbol (good-symbol-p expr:symbol)
  (:error-report :context))
