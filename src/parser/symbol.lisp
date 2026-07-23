(in-package :moonli)

(defun string-invert-case (string)
  (declare (optimize speed)
           (type string string))
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

(define-condition wrong-symbol-package (moonli-may-be-parse-error)
  ((actual :initarg :actual)
   (expected :initarg :expected))
  (:report (lambda (c s)
             (with-slots (actual expected) c
               (format s "Did you mean to use ~S instead of ~S?"
                       expected actual)))))


(defvar *moonli-macro-functions* (make-hash-table))
(defun expand-moonli-macro (expression)
  (funcall (car (gethash (first expression) *moonli-macro-functions*))
           (rest expression)))

(defvar *moonli-short-macro-functions* (make-hash-table))
(defun expand-moonli-short-macro (expression)
  (funcall (car (gethash (first expression) *moonli-short-macro-functions*))
           (rest expression)))

(defvar *moonli-infix-macro-functions* (make-hash-table))
(defun expand-moonli-infix-macro (expression)
  (funcall (car (gethash (first expression) *moonli-infix-macro-functions*))
           (rest expression)))


(esrap:defrule expr:symbol
    (or (and #\: simple-symbol)
        (and simple-symbol #\: simple-symbol)
        (and simple-symbol))
  (:around ()
    (block macro-check
      (let ((symbol (esrap:call-transform)))
        (maphash (lambda (key value)
                   (declare (ignore value))
                   (when (and (string-equal key symbol)
                              (not (eq (symbol-package key)
                                       (symbol-package symbol))))
                     (restart-case
                         (signal 'wrong-symbol-package
                                 :actual symbol :expected key)
                       (continue (&optional c)
                         (declare (ignore c))
                         (return-from macro-check symbol)))))
                 *moonli-macro-functions*)
        (maphash (lambda (key value)
                   (declare (ignore value))
                   (when (and (string-equal key symbol)
                              (not (eq (symbol-package key)
                                       (symbol-package symbol))))
                     (restart-case
                         (signal 'wrong-symbol-package
                                 :actual symbol :expected key)
                       (continue (&optional c)
                         (declare (ignore c))
                         (return-from macro-check symbol)))))
                 *moonli-short-macro-functions*)
        symbol)))
  (:lambda (expr esrap:&bounds start end)
    (let ((symbol
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
      (cond ((constantp symbol)
             (mark-syntax 'constant start end))
            ((find-class symbol nil)
             (mark-syntax 'type start end))
            ((not (good-symbol-p symbol))
             (mark-syntax 'keyword start end)))
      symbol))
  (:error-report :context))


(defun good-symbol-p (symbol)
  ;; Excluding these symbols is necessary, otherwise parser
  ;; cannot tell whether this symbol appears as part of a macro
  ;; or a variable or something else the
  (not (or (member symbol '(end elif else)
                   :test #'string-equal)
           (gethash symbol *moonli-macro-functions*)
           (gethash symbol *moonli-short-macro-functions*)
           (gethash symbol *moonli-infix-macro-functions*)
           (ignore-errors
            (parse-number:parse-number (symbol-name symbol))))))

(esrap:defrule good-symbol (good-symbol-p expr:symbol)
  (:error-report :context))
