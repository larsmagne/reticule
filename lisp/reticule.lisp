;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: RETICULE -*-

(defpackage reticule
  (:use :cl)
  (:export generate-password))

(in-package :reticule)

(setq *clocc-root* "/usr/local/lib/lisp/clocc/")
(load (concatenate 'string *clocc-root* "clocc"))
(load (translate-logical-pathname "clocc:src;cllib;base"))
(load (translate-logical-pathname "clocc:src;cllib;date"))

(defvar *reticule-home* "/home/larsi/reticule/data/")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :util "util")
  (require :hash "hash")
  (require :client "client")
  (require :server "server")
  )

(provide :reticule)
