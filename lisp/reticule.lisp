;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: RETICULE -*-

(defpackage reticule-system
  (:use :cl))

(in-package :reticule-system)

(asdf:defsystem reticule
	:version "0.1"
	:components ((:file "server")
		     (:file "client")
		     (:file "util")))

(provide :reticule)

 