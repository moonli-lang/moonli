(in-package :moonli)

(defvar *moonli-pprint-dispatch* (copy-pprint-dispatch))

(defun register-moonli-pprint-dispatch (type-specifier)
  (set-pprint-dispatch type-specifier
                       'moonli-pprint-object
                       0
                       *moonli-pprint-dispatch*))

(defgeneric moonli-pprint-object (stream object))

(register-moonli-pprint-dispatch 'cons)
(defmethod moonli-pprint-object (stream (o cons))
  (pprint-logical-block (stream o :prefix "(" :suffix ")")
    (loop :for i :below (or *print-length* (expt 2 64))
          :while o
          :do (when (null o) (return))
              (unless (consp o)
                (write-string " . " stream)
                (write o :stream stream)
                (return))
              (unless (zerop i)
                (write-string ", " stream)
                (pprint-newline :fill stream))
              (write (car o) :stream stream)
              (setf o (cdr o))
          :finally (when o (write-string ", ..." stream)))))

;; TODO: Respect print-length, etc

(defvar *moonli-hash-table-pprint-indent* 4)
(declaim (type fixnum *moonli-hash-table-pprint-indent*))

(register-moonli-pprint-dispatch 'hash-table)
(defmethod moonli-pprint-object (s (o hash-table))
  (pprint-logical-block (s nil)
    (write-char #\{ s)
    (pprint-logical-block (s nil)
      (pprint-indent :block *moonli-hash-table-pprint-indent* s)
      (pprint-newline :mandatory s)
      (let ((first t))
        (maphash
         (lambda (key value)
           (unless first
             (write-char #\, s)
             (pprint-newline :mandatory s))
           (setf first nil)
           (format s "~s : ~s" key value))
         o)))
    (pprint-newline :mandatory s))
  (write-char #\} s))

(register-moonli-pprint-dispatch 'symbol)
(defmethod moonli-pprint-object (s (o symbol))
  (when (keywordp o)
    (write-char #\: s))
  (write-string (string-invert-case (symbol-name o)) s))
