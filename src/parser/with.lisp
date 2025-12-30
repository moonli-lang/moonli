(in-package :moonli)

(5am:in-suite :moonli)

(esrap:defrule with-symbol
    (or (and #\: simple-symbol)
        (and simple-symbol #\: simple-symbol)
        (and simple-symbol))
  (:function (lambda (expr)
               (destructuring-bind (name pkg)
                   (optima:match expr
                     ((list package-name ":" symbol-name)
                      (let ((package (find-package package-name)))
                        (if package
                            (list symbol-name package)
                            (error (format nil "Package with name ~A does not exist while reading ~A:~A"
                                           package-name
                                           (string-invert-case package-name)
                                           (string-invert-case symbol-name))))))
                     ((list ":" symbol-name)
                      (list symbol-name :keyword))
                     ((list symbol-name)
                      (list symbol-name *package*)))
                 (let ((*package* pkg))
                   (alexandria:symbolicate 'with '- name))))))

(defun process-with-bindings (bindings body)
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind (first-name first-args) (first bindings)
        `(,first-name ,first-args
                      ,@(if (null (rest bindings))
                            body
                            (list (process-with-bindings (rest bindings)
                                                         body)))))))

(esrap:defrule with/binding
    (or (and expr:symbol
             +whitespace/internal
             "="
             +whitespace/internal
             with-symbol
             *whitespace/internal
             arglist)
        (and with-symbol
             *whitespace/internal
             arglist))
  (:function (lambda (args)
               (if (= 3 (length args))
                   `(,(first args)
                     ,(third args))
                   `(,(fifth args)
                     (,(first args) ,@(seventh args))))))
  (:error-report nil))

(esrap:defrule with/bindings
    (and with/binding
         *whitespace/all
         (* (and "," *whitespace/all with/binding *whitespace/all)))
  (:function (lambda (args)
               (cons (first args)
                     (mapcar #'third (third args)))))
  (:error-report nil))

(esrap:defrule with
    (and (or (and "with-"
                  simple-symbol
                  *whitespace/internal
                  arglist)
             (and "with"
                  +whitespace/all
                  with/bindings))
         *whitespace/internal
         ":"
         +whitespace/all
         moonli
         "end")
  (:function (lambda (expr)
               (optima:ematch expr
                 ((list name-bindings-form _ _ _ body _)
                  (optima:ematch name-bindings-form
                    ((list _ name _ args)
                     `(,(alexandria:symbolicate 'with '- name)
                       ,args
                       ,@(rest body)))
                    ((list _ _ bindings)
                     (process-with-bindings bindings (rest body)))))))))

(5am:def-test with ()
  (5am:is (equal `(with-open-file (f "/tmp/a.txt") f)
                 (esrap:parse 'with "with open-file(f, \"/tmp/a.txt\"): f end")))
  (5am:is (equal `(with-open-file (f "/tmp/a.txt") f)
                 (esrap:parse 'with "with-open-file(f, \"/tmp/a.txt\"): f end")))
  (5am:is (equal `(with-output-to-string (*standard-output*)
                    (with-open-file (f "/tmp/a.txt")
                      (write-line (read-line f))))
                 (esrap:parse 'with "with
    output-to-string(*standard-output*),
    open-file(f, \"/tmp/a.txt\"):
  write-line(read-line(f))
end")))
  (5am:is (equal `(alexandria:with-gensyms (a b c)
                    (list a b c))
                 (esrap:parse 'with "with alexandria:gensyms(a,b,c):
  list(a,b,c)
end")))
  (5am:is (equal `(alexandria:with-gensyms (a b c)
                    (with-open-file (f "/tmp/a.txt" :direction :output)
                      (write (list a b c) f)))
                 (esrap:parse 'with "with alexandria:gensyms(a,b,c),
    open-file(f, \"/tmp/a.txt\", :direction, :output):
  write(list(a,b,c), f)
end"))))
