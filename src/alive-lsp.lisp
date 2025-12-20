(in-package :moonli)

(pushnew (cons :moonli 'moonli-string-to-lisp-string)
         alive/packages:*parse-function-alist*
         :key #'car)

(pushnew (cons :moonli
               (lambda (stream)
                 (read-moonli-from-stream stream t)))
         alive/sys/eval:*read-function-alist*
         :key #'car)
