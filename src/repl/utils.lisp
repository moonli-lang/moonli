(in-package :moonli-user)

(defmacro help (name)
  (if (symbolp name)
      `(describe ',name)
      `(describe ,name)))

(defun run (&rest args)
  (apply #'uiop:run-program args))
