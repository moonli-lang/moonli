;;; This file was automatically generated.
;;; Do NOT edit by hand. It will be overwritten.
;;; Edit or Replace the corrsponding .moonli file instead!

;; Define a package: a namespace

(defpackage :moonli-sample
  (:use :cl)
  (:export :foo))

;; Change to that namespace

(in-package :moonli-sample)

;; Define a function

(defun foo (x) x)

;; Define a global variable

(defvar foo 'abc)

(print *package*)

(print foo)

(format t "hello! ~s: ~a~%" 'foo foo)

;; Define another global variable

(defparameter ht (moonli::fill-hash-table (:a 2) ("b" 3)))

(let ((a 1))
  a)

;; Define and use a new local variable 'a' again

(let ((a 1))
  (format t "a + gethash(:a, ht) + gethash(\"b\", ht) = ~a~%"
          (+ (+ a (gethash :a ht)) (gethash "b" ht))))

