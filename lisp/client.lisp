(in-package reticule)

(defvar *client-port* 8010)
(defvar *client-socket* nil)
(defvar *client-handler-process* nil)

(defvar *current-stream* nil)
(defvar *input-stream* nil)
(defvar *current-group* nil)
(defvar *current-article* nil)

(defvar *supported-commands*
  '(:group :article :head :body :list :post :xover :mode :help :quit))

(defun start-listening-for-clients ()
  (setq *client-socket*
	(extensions:create-inet-listener *client-port* :stream
					 :reuse-address t))
  (setq *client-handler-process* (mp:make-process #'accept-clients)))

(defun accept-clients ()
  (loop for fd = (ext:accept-tcp-connection *client-socket*)
	do
	(let ((stream
	       (system:make-fd-stream
		fd
		:input t
		:output t
		:element-type 'character)))
	  (multiple-value-bind (host port) (ext:get-peer-host-and-port fd)
	    (let ((host-entry (ext:lookup-host-entry host)))
	    (format t "Connection from ~a:~a~%"
		    (or (ext:host-entry-name host-entry)
			host)
		    port)))
	  (mp:make-process
	   #'(lambda ()
	       (handle-client stream)))
	  (mp:process-yield))))

(defun output-greeting ()
  (output "200 plane.gmane.org Gnumatone ready (posting ok)."))

(defun handle-client (stream)
  (let ((*current-stream* stream)
	(*current-group* nil))
    (format t "Handling ~a...~%" *current-stream*)
    (output-greeting)
    (loop for command = (read-command *current-stream*)
	  while (not (eq (car command) :quit))
	  when command
	  do (handle-command command))
    (output "205 closing connection - goodbye!")
    (close *current-stream*)
    (format t "Ended~%")))

(defun read-command (stream)
  (let ((line (read-line stream nil nil)))
    (if (null line)
	(list :quit)
      (progn
	(setq line (util:tokenize (string-right-trim (list #\return) line)))
	(if (null line)
	    nil
	  (progn
	    (setf (car line) (intern (string-upcase (car line)) :keyword))
	    line))))))

(defun output (&rest args)
  (apply 'format *current-stream* args)
  (princ #\return *current-stream*)
  (terpri *current-stream*)
  (force-output *current-stream*))

(defun nfoutput (&rest args)
  (apply 'format *current-stream* args)
  (princ #\return *current-stream*)
  (terpri *current-stream*))

(defun handle-command (spec)
  (let ((command (car spec))
	(parameters (cdr spec)))
    (format t "~a~%" spec)
    (if (member command *supported-commands*)
	(funcall (intern (format nil "HANDLE-~A-COMMAND" command) :reticule)
		 parameters)
      (output "500 Unknown command"))))

;;; Interface functions.

(defun handle-group-command (params)
  (let ((group (car params))
	active)
    (if (and (stringp group)
	     (setq group (string-downcase group))
	     (setq active (gethash group *active*)))
	(progn
	  (output "211 ~d ~d ~d ~a group selected"
		  (- (cadr active) (car active))
		  (car active)
		  (cadr active)
		  group)
	  (setq *current-group* group))
      (output "411 no such news group"))))

(defun handle-article-command (params)
  (handle-article params 'article))

(defun handle-head-command (params)
  (handle-article params 'head))

(defun handle-body-command (params)
  (handle-article params 'body))

(defun handle-article (params type)
  (let ((article (car params)))
    (cond
     ((null *current-group*)
      (output "412 no newsgroup has been selected"))
     ((null article)
      (if (null *current-article*)
	  (output "420 no current article has been selected")
	(output-article *current-group* *current-article* type)))
     (t
      (setq article (or (ignore-errors (parse-integer article))
			0))
      (let ((active (gethash *current-group* *active*)))
	(cond
	 ((not (<= (car active) article (cadr active)))
	  (output "423 no such article number in this group"))
	 ((not (probe-file (article-to-file *current-group* article)))
	  (output "430 no such article found"))
	 (t
	  (setq *current-article* article)
	  (output-article *current-group* article type))))))))

(defun output-article (group article type)
  (let* ((file (article-to-file group article))
	 (message-id (get-message-id-from-file file)))
    (output
     (cond
      ((eq type 'article)
       "220 ~d ~a article retrieved - head and body follows")
      ((eq type 'head)
       "221 ~d ~a article retrieved - head follows")
      ((eq type 'body)
       "222 ~d ~a article retrieved - body follows")
      (t
       (error "Unknown type")))
     article message-id)

    (with-open-file (article file
			     :direction :input)
      (loop for line = (read-line article nil nil)
	    while (and line
		       (not (zerop (length line))))
	    if (or (eq type 'article)
		   (eq type 'head))
	    do (output-encode-line line))

      (when (eq type 'article)
	(output-encode-line ""))

      (loop for line = (read-line article nil nil)
	    while line
	    if (or (eq type 'article)
		   (eq type 'body))
	    do (output-encode-line line)))

    (output ".")))

(defun output-encode-line (line)
  (when (and (not (zerop (length line)))
	     (eql (aref line 0) #\.))
    (princ "." *current-stream*))
  (princ line *current-stream*)
  (princ #\return *current-stream*)
  (terpri *current-stream*))

(defun get-message-id-from-file (file)
  (with-open-file (article file
			   :direction :input)
    (loop for line = (read-line article nil nil)
	  while (and line
		     (not (zerop (length line))))
	  do (setq line (util:tokenize line))
	  if (equalp (car line) "message-id:")
	  return (cadr line))))    

(defun handle-xover-command (params)
  (let ((range (car params))
	begin end active)
    (cond
     ((null *current-group*)
      (output "412 no newsgroup has been selected"))
     ((null range)
      (output "441 Invalid range"))
     ((not (= (length (setq range (util:tokenize range :delimiter #\-))) 2))
      (output "441 Invalid range"))
     (t
      (setq begin (or (ignore-errors (parse-integer (car range)))
		      0))
      (setq end (or (ignore-errors (parse-integer (cadr range)))
		    0))
      (setq active (gethash *current-group* *active*))
      (output "224 ~d-~d fields follow" begin end)
      (output-xover-lines *current-group* begin end)
      (output ".")))))

(defun output-xover-lines (group begin end)
  (let ((path (group-to-overview-file group))
	(current-base 0)
	(article 0)
	overview base line overview-file)
    (loop for i from begin upto end
	  do
	  (when (not (= current-base (setq base (1+ (truncate i 1000)))))
	    (when overview
	      (close overview))
	    (setq overview-file (format nil "~a.~d" path base))
	    (if (probe-file overview-file)
		(setq overview (open overview-file :direction :input)
		      current-base base)
	      (setq overview nil)))
	  (when overview
	    (loop while (and (< article i)
			     (setq line (read-line overview nil nil)))
		  do (setq article (read-from-string line)))
	    (when (= article i)
	      (output line))))
    (when overview
      (close overview))))

(defun handle-list-command (params)
  (let ((subcommand (car params)))
    (cond
     ((or (equalp subcommand "active")
	  (null subcommand))
      (if (null (cadr params))
	  (output-list-active)
	(output-list-active-group (cadr params))))
     (t
      (output "500 Unknown command")))))

(defun output-list-active ()
  (output "215 list of newsgroup follows")
  (maphash
   (lambda (group entry)
     (nfoutput "~a ~d ~d ~a" group (nth 1 entry) (nth 0 entry) (nth 2 entry)))
   *active*)
  (output "."))

(defun output-list-active-group (group)
  (output "215 list of newsgroup follows")
  (let ((entry (gethash group *active*)))
    (when entry
      (nfoutput "~a ~d ~d ~a" group (nth 1 entry)
		(nth 0 entry) (nth 2 entry))))
  (output "."))

(defun handle-mode-command (params)
  (output-greeting))

(defun handle-help-command (params)
  (let ((help
	 '("100 help text follows"
	   "  QUIT"
	   "  HELP"
	   "  GROUP group"
	   "  ARTICLE [Message-ID|Number]"
	   "  HEAD [Message-ID|Number]"
	   "  BODY [Message-ID|Number]"
	   "  MODE READER"
	   "  LIST ACTIVE [group]"
	   "  POST"
	   "  XOVER Number-Number"
	   "  STREAM From"
	   "  ACCEPT"
	   "Report problems to larsi@gnus.org"
	   ".")))
    (dolist (command help)
      (output command))))

(provide :client)
