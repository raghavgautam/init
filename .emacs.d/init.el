(setq ido-enable-flex-matching t
      ido-everywhere t)
(ido-mode 1)
(put 'narrow-to-region 'disabled nil)
(setq auto-mode-alist (cons '("\\.txt$" . auto-revert-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.log$" . auto-revert-tail-mode) auto-mode-alist))
(setq dired-auto-revert-buffer t)

(setq stack-trace-on-error nil) ;;it becomes irritating
(require 'net-utils)
(defun oozie-set-host (host-name)
  "Get Oozie info. You can try setting `oozie-info-options'."
  (interactive
   (list (read-from-minibuffer "Oozie host name: " system-name)))
  (setq oozie-port "11000"
	oozie-url (concat "http://" host-name ":" oozie-port "/oozie")
	oozie-bin "/usr/bin/oozie"
	oozie-job-options (list "job" "-oozie" oozie-url)
	oozie-info-options (append oozie-job-options (list "-info"))
	oozie-def-options (append oozie-job-options (list "-definition"))
	oozie-log-options (append oozie-job-options (list "-log")))
  (if (string= system-type "windows-nt")
      (setq oozie-bin (car (file-expand-wildcards "d:\hdp\oozie*\oozie-win-distro\bin\oozie"))))
  (if (fboundp 'net-utils-run-program)
      (fset 'oozie-run-func 'net-utils-run-program)
    (fset 'oozie-run-func 'net-utils-run-simple)))

(oozie-set-host system-name)

(defun oozie-utils-job-at-point ()
  (let ((pt (point)))
    (buffer-substring-no-properties
     (save-excursion
       (skip-chars-backward "-a-zA-Z0-9@")
       (point))
     (save-excursion
       (skip-chars-forward "-a-zA-Z0-9@")
       ;;(skip-chars-backward "." pt)
       (point)))))
;;(oozie-utils-job-at-point)000-abc-00-00@1

(defun o-info (job-name)
  "Get Oozie info. You can try setting `oozie-info-options'."
  (interactive
   (list (read-from-minibuffer "Oozie job name: " (oozie-utils-job-at-point))))
  (let* ((options (append oozie-info-options (list job-name)))
	 (buf-name (concat "oozie-info"))
	 (top-line (format "%s %s" oozie-bin (mapconcat 'identity options " "))))
    (switch-to-buffer (concat "*" buf-name "*"))
    (oozie-run-func
     buf-name
     top-line
     oozie-bin
     options)))

(defun o-log (job-name)
  "Get Oozie log. You can try setting `oozie-log-options'."
  (interactive
   (list (read-from-minibuffer "Oozie job name: " (oozie-utils-job-at-point))))
  (let* ((options (append oozie-log-options (list job-name)))
	 (buf-name (concat "oozie-log"))
	 (top-line (format "%s %s" oozie-bin (mapconcat 'identity options " "))))
    (switch-to-buffer (concat "*" buf-name "*"))
    (oozie-run-func
     buf-name
     top-line
     oozie-bin
     options)))

(defun o-def (job-name)
  "Get Definition of Oozie job. You can try setting `oozie-def-options'."
  (interactive
   (list (read-from-minibuffer "Oozie job name: " (oozie-utils-job-at-point))))
  (let* ((options (append oozie-def-options (list job-name)))
	 (buf-name (concat "oozie-def"))
	 (top-line (format "%s %s" oozie-bin (mapconcat 'identity options " "))))
    (switch-to-buffer (concat "*" buf-name "*"))
    (oozie-run-func
     buf-name
     top-line
     oozie-bin
     options)))

(setq yarn-log-options '("logs" "-applicationId"))

(defun yarn-utils-job-at-point ()
  (let ((pt (point)))
    (buffer-substring-no-properties
     (save-excursion
       (skip-chars-backward "_0-9")
       (point))
     (save-excursion
       (skip-chars-forward "_0-9")
       ;;(skip-chars-backward "." pt)
       (point)))))

(defun y-log (job-name)
  "Get yarn log."
  (interactive
   (list (read-from-minibuffer "Oozie job name: " (yarn-utils-job-at-point))))
  (let* ((options (append yarn-log-options (list (concat "application" job-name))))
	 (buf-name (concat "yarn-logs"))
	 (yarn-bin "yarn")
	 (top-line (format "%s %s" yarn-bin (mapconcat 'identity options " "))))
    (oozie-run-func
     buf-name
     top-line
     yarn-bin
     options)))

(defun write-in-tmp (file-name)
  "Write the buffer in /tmp/ dir"
  (interactive
   (list
    (read-from-minibuffer "Write to file: " (concat "/tmp/" (replace-regexp-in-string "*" "" (buffer-name)) ".txt"))))
  (write-file file-name))

(defun collect-text (beg end &optional region)
  "Collect selected text in a *collect* buffer"
  (interactive (list (mark) (point)
		     (prefix-numeric-value current-prefix-arg)))
  (append-to-buffer "*collect*" beg end)
  (message "Copied the selected text to buffer *collect*")
  (indicate-copied-region))

(global-set-key "\C-col" 'o-log)
(global-set-key "\C-cod" 'o-def)
(global-set-key "\C-coi" 'o-info)
(global-set-key "\C-cos" 'oozie-set-host)
(global-set-key "\C-cyl" 'y-log)
(global-set-key "\C-cc" 'collect-text)
(global-set-key "\C-cr" 'write-in-tmp)
;;(find-file (make-temp-file "foo"))

(defun rename-buffer-maybe-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (get-buffer new-name)
	(message "A buffer named '%s' already exists!" new-name)
      (progn
	(cond (filename
	       (progn (rename-file name new-name 1)
		      (rename-buffer new-name)
		      (set-visited-file-name new-name)
		      (set-buffer-modified-p nil)))
	      (t (rename-buffer (concat "*" new-name "*"))))))))

(global-set-key [f2] 'rename-buffer-maybe-file)
