(in-package reticule)

(defvar *active* nil)
(defvar *spool* "/mirror/var/spool/news/")

(defvar *nov-headers*
  '(article subject from date message-id references bytes lines xref keywords))

(defvar *full-nov-headers*
  '(xref keywords))

(defstruct headers
  article
  subject
  from
  date
  message-id
  references
  bytes
  lines
  xref
  keywords)

(defun read-active-file ()
  (setq *active* (make-hash-table :test #'equalp))
  (with-open-file (active "/var/lib/news/active"
			  :direction :input)
    (loop for line = (read-line active nil nil)
	  while line
	  do
	  (setq line (util:tokenize line))
	  (setf (gethash (car line) *active*)
		(list (parse-integer (nth 2 line))
		      (parse-integer (nth 1 line))
		      (nth 3 line))))))
		      
(defun article-to-file (group article)
  (format nil "~aarticles/~a/~d" *spool* (substitute #\/ #\. group) article))

(defun group-to-directory (group)
  (format nil "~aarticles/~a/" *spool* (substitute #\/ #\. group)))

(defun group-to-overview-file (group)
  (concatenate 'string (group-to-overview-directory group) group))

(defun group-to-overview-directory (group)
  (format nil "~aoverview/~a/"
	  *spool*
	  (util:mapconcatenate
	   (mapcar
	    (lambda (dir)
	      (format nil "~c" (aref dir 0)))
	    (util:tokenize group :delimiter #\.))
	   "/")))

(defun write-nov (headers stream)
  (dolist (slot *nov-headers*)
    (if (member slot *full-nov-headers*)
	(when (slot-value headers slot)
	  (princ (string-capitalize (symbol-name slot)) stream)
	  (princ ": " stream)
	  (princ (slot-value headers slot) stream)
	  (princ #\tab stream))
      (progn
	(princ (or (slot-value headers slot) "") stream)
	(princ #\tab stream))))
  (terpri stream))

(defun parse-headers (file)
  (let ((headers (make-headers))
	(bytes 0)
	(lines 0)
	colon header)
    (with-open-file (article file
			     :direction :input)
      (loop with full-line = ""
	    for line = (read-line article nil nil)
	    do
	    (if (or (not line)
		    (zerop (length line))
		    (and (not (eql (aref line 0) #\space))
			 (not (eql (aref line 0) #\tab))))
		(progn
		  (unless (zerop (length full-line))
		    (when (setq colon (position #\: full-line))
		      (when (member
			     (setq header (intern
					   (string-upcase
					    (subseq full-line 0 colon))
					   'reticule))
			     *nov-headers*)
			(setf (slot-value headers header)
			      (substitute #\space #\tab
					  (string-left-trim
					   " "
					   (subseq full-line (1+ colon))))))))
		  (setq full-line line))
	      (setq full-line (concatenate 'string full-line " " line)))
	    while (and line
		       (not (zerop (length line)))))

      (loop for line = (read-line article nil nil)
	    while line
	    do
	    (incf lines)
	    (incf bytes (1+ (length line))))

      (setf (slot-value headers 'bytes) bytes)
      (setf (slot-value headers 'lines) lines))
    
    headers))

(defun articles-in-directory (directory)
  (sort
   (loop with article = nil
	 for file in (directory directory :check-for-subdirs nil)
	 when (setq article
		    (ignore-errors (parse-integer (file-namestring file))))
	 collect article)
   #'<))

(defun rebuild-overview (group)
  (let ((directory (group-to-directory group))
	(overview-directory (group-to-overview-directory group))
	(current-base 0)
	headers overview base overview-file)
    (util:ensure-directory overview-directory)
    (dolist (article (articles-in-directory directory))
      (format t "~a~%" article)
      (setq headers (parse-headers (format nil "~a~d" directory article)))
      (setf (slot-value headers 'article) article)

      (when (not (= current-base (setq base (1+ (truncate article 1000)))))
	(when overview
	  (close overview)
	  (unix:unix-rename (concatenate 'string overview-file ".tmp")
			    overview-file))
	(setq overview (open (concatenate
			      'string
			      (setq overview-file
				    (format nil "~a/~a.~d"
					    overview-directory group base))
			      ".tmp")
			     :direction :output
			     :if-does-not-exist :create
			     :if-exists :overwrite)
	      current-base base))
      (write-nov headers overview))
    (when overview
      (close overview)
      (unix:unix-rename (concatenate 'string overview-file ".tmp")
			overview-file))))

(provide :server)
