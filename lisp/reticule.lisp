;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: MEDUSA -*-

(defpackage medusa
  (:use :cl)
  (:export generate-password))

(in-package :medusa)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :util "/home/larsi/medusa/lisp/util")
  (require :client "/home/larsi/medusa/lisp/client")
  (require :server "/home/larsi/medusa/lisp/server")
  )

(provide :medusa)

 