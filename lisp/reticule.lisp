;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: MEDUSA -*-

(defpackage medusa-system
  (:use :cl))

(in-package :medusa-system)

(asdf:defsystem medusa
	:version "0.1"
	:components ((:file "server")
		     (:file "client")
		     (:file "util")))

(provide :medusa)

 