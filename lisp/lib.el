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

(and nil
     (let ((r "\\(?:a\\)\\(BC\\)\\(?:d\\)")
           (s "_aBCd_")
           (pos 1))
       (string-match r s)
       (match-string pos s))
     )
(defun find-in-file (file-name regex num)
  "Find content in a FILE-NAME by REGEX & NUM."
  (unless (file-regular-p path file-name)
    (user-error "Supplied filename is not a file: %s" file-name))
  (save-window-excursion
    (with-current-buffer (find-file file-name)
      (goto-char (point-min))
      (re-search-forward regex)
      (match-string-no-properties num))))


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

(defun get-proj-root ()
  (interactive)
  (message default-directory))

(defun my-url-decoder (url)
  (interactive
   (list (read-from-minibuffer "URL: " (thing-at-point 'url))))
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

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(provide 'lib)
