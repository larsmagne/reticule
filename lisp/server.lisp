(in-package reticule)

(defvar *active* nil)
(defvar *spool* "/mirror/var/spool/news/")

(defvar *nov-headers*
  '(article subject from date message-id references
	    bytes lines xref gmane-status x-article))

(defvar *nov-size* 256)

(defvar *group-name-table* nil)
(defvar *group-name-list* nil)
(defvar *max-group-id* 0)
(defvar *gmane-conf* nil)
(defvar *history-file* nil)
(defvar *active-file* nil)

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
  gmane-status
  x-archive
  suppressed)

(defstruct conf
  local
  group
  address
  description
  type
  parameters)

(defun initialize ()
  (setq *group-name-table* (make-hash-table :test #'equalp))
  (read-group-name-table)
  (setq *gmane-conf* (read-gmane-conf))
  (add-new-groups)
  (write-group-name-table)

  (hash:open-hash (concatenate 'string *reticule-home* "index"))
  (setq *history-file* (open-history
			(concatenate 'string *reticule-home* "history")))
  (setq *active-file* (open-active
		       (concatenate 'string *reticule-home* "history"))))

(defun open-history (file-name)
  (open file-name :direction :io
	:if-exists :append
	:if-does-not-exist :create))

(defun open-active (file-name)
  (open file-name :direction :io
	:if-exists :overwrite
	:if-does-not-exist :create))

(defun read-group-name-table ()
  (setq *group-name-list* nil)
  (let ((path (concatenate 'string *reticule-home* "groups.dat"))
	id)
    (when (probe-file path)
      (with-open-file (groups path
			      :direction :input)
	(loop for line = (read-line groups nil nil)
	      while line
	      do
	      (setq line (util:tokenize line)
		    id (parse-integer (cadr line)))
	      (setf (gethash (car line) *group-name-table*) id)
	      (push (cons (car line) id)
		    *group-name-list*)
	      (setq *max-group-id* id))))))

(defun write-group-name-table ()
  (let ((path (concatenate 'string *reticule-home* "groups.dat")))
    (with-open-file (groups path
			    :direction :output
			    :if-exists :rename
			    :if-does-not-exist :create)
      (dolist (elem (reverse *group-name-list*))
	(format groups "~a ~d~%" (car elem) (cdr elem))))))

(defun valid-conf-line-p (line)
  (and (> (length line) 4)
       (not (eql #\# (aref (car line) 0)))))

(defun read-gmane-conf ()
  (with-open-file (conf "/etc/gmane.conf"
			  :direction :input)
    (loop with start = nil
	  for line = (read-line conf nil nil)
	  while line
	  do (setq line (util:tokenize line :delimiter #\:))
	  when (and start
		    (valid-conf-line-p line))
	  collect (handle-conf-line line)
	  when (string= (car line) "groups=")
	  do (setq start t))))

(defun handle-conf-line (line)
  (let ((conf (make-conf
	       :local (nth 0 line)
	       :group (nth 1 line)
	       :address (nth 2 line)
	       :description (nth 3 line)
	       :type (nth 5 line)
	       :parameters (mapcar (lambda (elem)
				     (util:tokenize elem :delimiter #\=))
				   (nthcdr 5 line)))))
    conf))

(defun add-new-groups ()
  (dolist (group (append (loop for conf in *gmane-conf*
			       collect (slot-value conf 'group))
			 (read-active-file)))
    (unless (gethash group *group-name-table*)
      (let ((next (incf *max-group-id*)))
	(setf (gethash group *group-name-table*) next)
	(push (cons group next) *group-name-list*)))))

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
		      (nth 3 line)))
	  collect (car line))))

(defun write-active-file ()
  (dolist (group (mapcar #'car *group-name-list*))
    (let ((active (gethash group *active*)))
      (write-active-entry group
			  (or (car active) 0)
			  (or (cadr active) 0) 0)))
  (force-output *active-file*))

(defun write-active-entry (group low high number)
  (file-position *active-file* (* (group-name-to-group-id group) 4 3))
  (write-integer *active-file* low 4)
  (write-integer *active-file* high 4)
  (write-integer *active-file* number 4)
  )

(defun article-to-file (group article)
  (format nil "~aarticles/~a/~d" *spool* (substitute #\/ #\. group) article))

(defun group-to-directory (group)
  (format nil "~aarticles/~a/" *spool* (substitute #\/ #\. group)))

(defun group-to-overview-file (group)
  (concatenate 'string (group-to-overview-directory group) group))

(defun group-to-overview-directory (group)
  (format nil "~aoverview/~a/"
	  *reticule-home*
	  (util:mapconcatenate
	   (mapcar
	    (lambda (dir)
	      (format nil "~c" (aref dir 0)))
	    (util:tokenize group :delimiter #\.))
	   "/")))

(defun write-integer (s value bytes)
  (let ((result nil))
    (dotimes (i bytes)
      (push (code-char (mod value 256)) result)
      (setq value (truncate value 256)))
    (dolist (byte (nreverse result))
      (princ byte s))))

(defun group-name-to-group-id (group-name)
  (gethash group-name *group-name-table* 0))

(defun crosspost-spec (xref)
  (loop with elem = 0
	for spec in (cdr (util:tokenize xref))
	do (setq elem (util:tokenize spec :delimiter #\:))
	when elem
	collect (cons (group-name-to-group-id (car elem))
		      (parse-integer (cadr elem)))))

(defun status-byte (headers crosspost-spec)
  (+ (length crosspost-spec)
     128 ; present
     (* 64 (if (slot-value headers 'suppressed) 1 0))
     (* 32 (if (assoc (group-name-to-group-id "gmane.spam.detected")
		      crosspost-spec)
	       1 0))))

(defun generate-nov (headers)
  (with-output-to-string (s)
    (let ((crosspost-spec (crosspost-spec (slot-value headers 'xref))))
      (write-integer s (status-byte headers crosspost-spec) 1)

      (dolist (spec crosspost-spec)
	(write-integer s (car spec) 2)
	(write-integer s (cdr spec) 4))

      (write-integer s
		     (or (ignore-errors
			   (cllib:string->dttm (slot-value headers 'date)))
			 0)
		     4)

      (write-integer s (slot-value headers 'lines) 4)
      (write-integer s (slot-value headers 'bytes) 4)

      (dolist (field '(subject from message-id references))
	(let ((value (slot-value headers field)))
	  (princ (or value "") s)
	  (write-integer s 0 1))))))

(defun write-nov (headers stream)
  (file-position stream (* (slot-value headers 'article) *nov-size*))

  (let ((block
	    (loop for block = (generate-nov headers)
		  while (> (length block) *nov-size*)
		  do (setq headers (prune-headers headers))
		  finally (return block))))
    (princ block stream)
    (princ (make-string (- *nov-size* (length block))
			:initial-element #\nul)
	   stream)))

(defun prune-references (references)
  (let ((ids (util:tokenize references)))
    (cond
     ((> (length ids) 2)
      (setf (cdr ids) (cddr ids)))
     ((= (length ids) 2)
      (setq ids (cdr ids))))
    (util:mapconcatenate ids " ")))

(defun prune-headers (headers)
  (with-slots (references subject from) headers
    (cond
     ((find #\space references)
      (setf (slot-value headers 'references)
	    (prune-references references)))
     ((> (length subject) 10)
      (setf (slot-value headers 'subject)
	    (subseq subject 0 (- (length subject) 10))))
     ((> (length from) 10)
      (setf (slot-value headers 'from)
	    (subseq from 0 (- (length from) 10))))))
  headers)

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

(defun articles-in-directory-1 (directory)
  (loop with article = nil
	for file in (directory
		     (concatenate 'string directory "/*")
		     :check-for-subdirs nil)
	do (setq article (file-namestring file))
	when (file-article-p article :suppressed t)
	collect article))

(defvar *directory-cache* (make-hash-table :test #'equal))

(defun articles-in-directory (directory)
  (or (gethash directory *directory-cache* nil)
      (let ((articles (articles-in-directory-1 directory)))
	(setf (gethash directory *directory-cache*) articles)
	articles)))
  
(defun file-article-p (file-name &key (suppressed nil))
  (every (lambda (char)
	   (or (and suppressed
		    (eql char #\-))
	       (and (>= (char-code char)
			(char-code #\0))
		    (<= (char-code char)
			(char-code #\9)))))
	 file-name))

(defun article-file-name-to-article (file-name)
  (if (file-article-p file-name)
      (parse-integer file-name)
    (parse-integer (subseq file-name 0 (1- (length file-name))))))

(defun articles-in-directory-old (directory)
  (sort
   (loop with article = nil
	 for file in (directory
		      (concatenate 'string directory "/*")
		      :check-for-subdirs nil)
	 when (setq article
		    (ignore-errors (parse-integer (file-namestring file))))
	 collect article)
   #'<))

(defun add-to-map (map article)
  (let ((index (truncate (/ article 8))))
    (when (>= index (length map))
      (setq map (concatenate 'string map
			     (make-string (1+ (- index (length map)))
					  :initial-element #\nul))))
    (setf (aref map index)
	  (code-char (logior (char-code (aref map index))
			     (ash 1 (1- (mod article 8))))))
    map))

(defun rebuild-overview (group)
  (let ((directory (group-to-directory group))
	(overview-directory (group-to-overview-directory group))
	(map "")
	headers overview overview-file file-name map-file)
    (util:ensure-directory overview-directory)
    (setq overview (open (concatenate
			  'string
			  (setq overview-file
				(format nil "~a/~a.NOV"
					overview-directory group))
			  ".tmp")
			 :direction :output
			 :if-does-not-exist :create
			 :if-exists :overwrite))

    (dolist (article (articles-in-directory directory))
      (format t "~a~%" article)
      (setq file-name (format nil "~a~d" directory article))
      (setq headers (parse-headers file-name))
      (setf (slot-value headers 'article)
	    (article-file-name-to-article article))
      (setf (slot-value headers 'suppressed)
	    (not (file-article-p article)))

      (write-nov headers overview)

      (unless (slot-value headers 'suppressed)
	(setq map (add-to-map map (slot-value headers 'article)))))
    
    (close overview)
    (unix:unix-rename (concatenate 'string overview-file ".tmp")
		      overview-file)
    (with-open-file (mapf (concatenate 'string
				       (setq map-file
					     (format nil "~a/~a.MAP"
						     overview-directory group))
				       ".tmp")
			  :direction :output
			  :if-exists :rename
			  :if-does-not-exist :create)
      (princ map mapf))
    (unix:unix-rename (concatenate 'string map-file ".tmp")
		      map-file)))

(provide :server)
