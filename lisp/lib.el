(setq tmp-buf "temp")

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))


(defun run-cmd-get-output (command &rest args)
  "Run the COMMAND get string output"
  (apply #'call-process command nil tmp-buf nil args)
  (switch-to-buffer tmp-buf)
  (delete-other-windows)
  (with-current-buffer tmp-buf
    (let ((output (buffer-string)))
      (kill-buffer)
      (trim-string output))))

(defun get-line-number-file ()
  "Get line number of the in file."
  (number-to-string (save-excursion
		      (save-restriction
			(widen)
			(line-number-at-pos)))))

;;(get-line-number-file)

(defun robot-input ()
  (run-cmd-get-output  "log-robot"
		       "--command"
		       "get-robot-input"
		       "--file"
		       (buffer-file-name)
		       "--line"
		       (get-line-number-file)
		       "--column"
		       (number-to-string (current-column))))

(defun robot-record-feedback (feedback)
  (run-cmd-get-output  "log-robot"
		       "--command"
		       "record-user-feedback"
		       "--file"
		       (buffer-file-name)
		       "--line"
		       (get-line-number-file)
		       "--column"
		       (number-to-string (current-column))
		       "--feedback"
		       feedback))
(defun ask-user ()
  (interactive)
  "Ask user for bug confirmation"
  (message
   (concat 
    "Robot response: "
    (robot-record-feedback
     (ido-completing-read
      "Choose: "
      (split-string (trim-string (robot-input)) "\n"))))))


;;url decoding help
(defun url-at-point ()
  (let ((pt (point)))
    (buffer-substring-no-properties
     (save-excursion
       (skip-chars-backward "-a-zA-Z0-9@:/.?&=%_*")
       (point))
     (save-excursion
       (skip-chars-forward "-a-zA-Z0-9@:/.?&=%_*")
       ;;(skip-chars-backward "." pt)
       (point)))))

(defun get-proj-root ()
  (interactive)
  (message default-directory))

(defun my-url-decoder (url)
  (interactive
   (list (read-from-minibuffer "URL: " (url-at-point))))
  "Decode a url and show it params and parts"
  (let* ((temp1 (split-string (url-unhex-string url) "?"))
	 (retval (car temp1))
	 (args (split-string (cadr temp1) "&")))
    (progn
      (while args
	(setq retval (concat retval "\n" (car args)))
	(setq args (cdr args)))
      (message retval)
      retval
      )))

;;(my-url-decoder "http://ip-172-31-35-204.ec2.internal:15000/api/instance/list/process/agregator-coord16-02b73cdc?start=2014-10-31T23%3A18Z&end=2014-10-31T23%3A44Z&filterBy=status%3ARUNNING&user.name=hrt_qa")
