;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: HASH -*-

(defpackage hash
  (:use :cl)
  (:export open-hash close-hash lookup enter))

(in-package :hash)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :util "util"))

(defvar *hash-file* nil)
(defvar *hash-size* nil)

(defun create-empty-hash (file-name &key (blocks (* 1024)))
  (let ((block (concatenate 'string "-"
			    (make-string 1023 :initial-element #\space))))
    (setq *hash-size* (* blocks 1024))
    (with-open-file (file file-name
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists :overwrite)
      (dotimes (i blocks)
	(princ block file)))))

(defun open-hash (file-name &key (forcep t))
  (when (or (not (probe-file file-name))
	    forcep)
    (create-empty-hash file-name))
  (setq *hash-file* (open file-name :direction :io
			  :if-exists :overwrite)))

(defun close-hash ()
  (close *hash-file*)
  (setq *hash-file* nil))

(defun hash (string)
  (mod (reduce #'+ (map 'list #'char-code string)) (/ *hash-size* 1024)))

(defun lookup (string)
  (let ((block (* 1024 (hash string)))
	(id (intern string))
	(element '+))
    (file-position *hash-file* block)
    (loop while (or (eq element '+)
		    (and (consp element)
			 (not (eq (car element) id))))
	  do
	  (setq element (read *hash-file*))
	  (format t "Got element ~a~%" element)
	  (when (eq element '+)
	    (incf block 1024)
	    (when (> block *hash-size*)
	      (setq block 0))
	    (file-position *hash-file* block)))
    (file-position *hash-file* (1- (file-position *hash-file*)))
    (values (if (consp element) element nil) (file-position *hash-file*))))

(defun enter (string value)
  (multiple-value-bind (element position) (lookup string)
    (if (consp element)
	element
      (let ((rep (format nil "~s" (cons (intern string) value))))
	(loop while (> (length rep) (- 1024 (mod position 1024)))
	      do
	      (format t "Going to the next block~%")
	      (file-position *hash-file* (decf position))
	      (princ '+ *hash-file*)
	      ;; Go to the start of the next block.
	      (let ((new-pos (+ position (- 1024 (mod position 1024)))))
		(when (> new-pos *hash-size*)
		  (setq new-pos 0))
		(file-position *hash-file* new-pos))
	      ;; Find the end.
	      (loop while (consp (read *hash-file*)))
	      (setq position (file-position *hash-file*)))
	(format t "entering ~a ~a~%" rep position)
	(file-position *hash-file* (decf position))
	(princ rep *hash-file*)
	(princ '- *hash-file*)
	(values rep (file-position *hash-file*))
	(force-output *hash-file*)))))

(provide :hash)
