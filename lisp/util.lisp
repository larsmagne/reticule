;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: UTIL -*-

(defpackage util
  (:use :cl)
  (:export :pp :tokenize :restrict-string :shell-command
	   :with-bindings :mapconcatenate :dirname :basename :money
	   :ensure-directory :name :limit-string :eval-from-file
	   :sql-from-file :format-table :plist-to-alist
	   :with-binding
	   :format-vps-account :format-bank-account :format-birth-number))

(in-package :util)

(eval-when (:load-toplevel :compile-toplevel :execute)
  )

(defun pp (form)
  (let ((*print-pretty* t))
    (prin1 form)
    (terpri))
  nil)

(defun tokenize (string &key (delimiter #\Space)
			(remove-trailer-p t)
			(trailer #\Space))
  (let ((tlist nil)
	(pos1 0)
	(pos2 0))
    (do ()
	((not (setq pos2 (position delimiter string :start pos1))))
      (unless (eql pos1 pos2)
	(push (subseq string pos1 pos2) tlist))
      (setq pos1 (1+ pos2)))
    (when (< pos1 (length string))
      (let ((laststr (subseq string pos1)))
	(when remove-trailer-p 
	  (setq laststr (delete trailer laststr)))
	(push laststr tlist)))
    (nreverse tlist)))

(defun restrict-string (string length)
  (if (> (length string) length)
      (subseq string 0 length)
    string))

(defun shell-command (command)
  (with-output-to-string (*standard-output*)
    #+lispworks
    (sys:call-system-showing-output
     command
     :prefix ""
     :show-cmd nil
     :shell-type "/bin/sh")))

(defun basename (path)
  (car (last (tokenize path :delimiter #\/))))

(defun dirname (path)
  (let ((elems (tokenize path :delimiter #\/)))
    (setf (nthcdr (1- (length elems)) elems) nil)
    (mapconcatenate elems "/")))

(defmacro with-bindings (bindings statement &rest forms)
  `(dolist (elem ,statement)
     (destructuring-bind ,bindings elem
       ,@forms)))

(defmacro with-binding (bindings statement &rest forms)
  `(destructuring-bind ,bindings (car ,statement)
     ,@forms))

(defun mapconcatenate (list separator)
  (with-output-to-string (s)
    (do ((list list (cdr list)))
	((not list))
      (princ
       (if (cdr list)
	   (concatenate 'string (car list) separator)
	 (car list))
       s))))

;; Make sure that DIRECTORY exists.
(defun ensure-directory (directory)
  (let ((path "")
	(created nil))
    (dolist (elem (tokenize directory :delimiter #\/))
      (setq path (concatenate 'string path "/" elem))
      (unless (probe-file path)
	(setq created t)
	(unix:unix-mkdir path 509)))
    created))

(defun name (first-name last-name)
  (if (string= first-name "")
      last-name
    (concatenate 'string first-name " " last-name)))

(defun money (amount)
  (format nil "~,2f" amount))

(defun limit-string (string length)
  (if (> (length string) length)
      (subseq string 0 length)
    string))

(defun eval-from-file (file-name)
  (with-open-file (file file-name :direction :input)
    (loop with line
	  while (setq line (read-line file nil nil))
	  do
	  (format t line)
	  (terpri)
	  (eval (read-from-string line)))))

(defun plist-to-alist (plist)
  (loop while plist
	collect (list (car plist) (cadr plist))
	do (setq plist (cddr plist))))

(defun table-plist-to-alist (plist)
  (loop while plist
	collect (if (symbolp (car plist))
		    (pop plist)
		  (prog1
		      (list (car plist) (cadr plist))
		    (setq plist (cddr plist))))))

(defun format-table (stream &rest pairs)
  (let ((alist (table-plist-to-alist pairs))
	(width 0))
    (dolist (elem alist)
      (when (consp elem)
	(setq width (max width (length (car elem))))))

    (incf width 5)
    
    (dolist (elem alist)
      (cond
       ((consp elem)
	(princ (car elem) stream)
	(princ ":" stream)
	(princ (make-string (- width (length (car elem)))
			    :initial-element #\Space)
	       stream)
	(princ (cadr elem) stream)
	(terpri stream))
       (t
	(cond
	 ((eq elem :blank)
	  (terpri stream))
	 (t
	  (error "Unknown directive"))))))))

(defun format-vps-account (account)
  (concatenate 'string (subseq account 0 5) "." (subseq account 5)))

(defun format-bank-account (account)
  (concatenate 'string (subseq account 0 4) "."
	       (subseq account 4 6) "."
	       (subseq account 6)))

(defun format-birth-number (number)
  (concatenate 'string (subseq number 0 6) " " (subseq number 6)))


(provide :util)

