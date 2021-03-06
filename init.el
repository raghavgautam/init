;; Licensed to the Apache Software Foundation (ASF) under one
;; or more contributor license agreements.  See the NOTICE file
;; distributed with this work for additional information
;; regarding copyright ownership.  The ASF licenses this file
;; to you under the Apache License, Version 2.0 (the
;; "License"); you may not use this file except in compliance
;; with the License.  You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; Author: Raghav Kumar Gautam <raghavgautam@gmail.com>

(unless (bound-and-true-p laptop)
  (setq ido-enable-flex-matching t
      ido-everywhere t)
  (ido-mode 1))

(put 'narrow-to-region 'disabled nil)


(setq url-proxy-services '(("no_proxy" . ".*")))
(let* ((pkg-dir (expand-file-name "~/.emacs.d/elpa/"))
       (pkg-file (concat pkg-dir "package.el")))
  (when (equal emacs-major-version 23)
    (if (load pkg-file t)
	(package-initialize)
      (let ((buffer
	     (url-retrieve-synchronously "http://git.savannah.gnu.org/gitweb/?p=emacs.git;a=blob_plain;hb=ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09;f=lisp/emacs-lisp/package.el")))
	(if (not buffer)
	    (message "Attempt to fetch package-install.el failed.")
	  (with-current-buffer buffer
	    (goto-char (point-min))
	    (re-search-forward "^$" nil 'move)
	    (delete-region (point-min) (point))
	    (eval-region (point) (point-max))
	    (mkdir pkg-dir t)
	    (write-file pkg-file nil)
	    (kill-buffer (current-buffer))))))))

(when (and (>= emacs-major-version 23) (not (bound-and-true-p laptop))  (require 'package nil t))
  (message "loading package manager stuff")
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (unless (require 'tramp-hdfs nil t)
    (ignore-errors
      (package-refresh-contents)
      (package-initialize)
      (package-install 'helm)
      (require 'helm)
      (package-install 'helm-jstack)
      (require 'helm-jstack)
      (package-install 'helm-wordnet)
      (require 'helm-wordnet)
      (package-install 'tramp-hdfs)
      (require 'tramp-hdfs)
      (package-install 'osx-lib)
      (require 'osx-lib))))

(defun rkg-keys ()
  (interactive)
  (require 'helm nil t)
  (ido-mode -1)
  (helm-mode 1)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

(defun default-keys ()
  (interactive)
  (require 'ido)
  (ido-mode 1)
  (ignore-errors (helm-mode -1))
  (global-set-key (kbd "C-x b") 'switch-to-buffer)
  (global-set-key (kbd "C-x C-f") 'find-file))

(unless (bound-and-true-p laptop)
  (default-keys))

(add-hook 'fr-mode-hook 'auto-revert-mode)
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
      (setq oozie-bin (car (file-expand-wildcards "d:/hdp/oozie*/oozie-win-distro/bin/oozie"))))
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
(defvar temp-dir
  (if (bound-and-true-p laptop)
      "~/tmp/"
    "/tmp/")
  "Temp directory to use.")
  
(defun write-in-tmp (file-name)
  "Write the buffer in /tmp/ dir"
  (interactive
   (list
    (read-from-minibuffer "Write to file: " (concat temp-dir (replace-regexp-in-string "*" "" (buffer-name)) ".txt"))))
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

;;(nslookup-host "74.125.239.40")

;(cdr (assoc 'result (json-read-from-string (fetch-json (concat "http://localhost:9000/test?param=" (url-hexify-string "ls -al ~/"))))))


(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
				    (: (* (any " \t\n")) eos)))
			    ""
			    str))
(defun pick-time-at-point ()
  (let ((pt (point)))
    (chomp
    (buffer-substring-no-properties
     (save-excursion
       (skip-chars-backward "-_: 0-9")
       (point))
     (save-excursion
       (skip-chars-forward "-_: 0-9")
       ;;(skip-chars-backward "." pt)
       (point))
     ))
    ))
    
;; (pick-time "INFO 2014-06-13 12:30:34,003 tahoth")
(defvar log-timestamp nil)

(defun pick-logtime (log-time)
  "Pick timestamp from log."
  (interactive
   (list (read-from-minibuffer "Picking time: " (pick-time-at-point))))
  (setq log-timestamp log-time))

(defun search-logtime ()
  (interactive
   (isearch-forward log-timestamp)))

(defcustom dir-ignore-list
  '("." ".." ".git" ".idea")
  "List of file/directories to ignore")

(defcustom dir-include-list
  '("/etc" "/mnt/hadoopqe" "/grid/0/hadoopqe" "/home/hrt_qa")
  "List of file/directories to include")

(defun rkg-ff ()
  (interactive)
  (find-file
   (ido-completing-read
    "Test:"
    (apply #'append  (mapcar 'directory-files-recursive dir-include-list)))))

(defun directory-files-recursive (dir)
  (setq dir (expand-file-name dir))
  (ignore-errors
    (cond ((not (file-exists-p dir)) nil)
	  ((not (file-accessible-directory-p dir)) nil)
	  ((file-regular-p dir) dir)
	  (t (let* ((file+dir (directory-files dir)))
	       (setq file+dir
		     (delete-if
		      (lambda (x) (member x dir-ignore-list))
		      file+dir))
	       (setq file+dir
		     (mapcar
		      (lambda (x)
			(concat (file-name-as-directory dir) x))
		      file+dir))
	       (let* ((files (remove-if-not 'file-regular-p file+dir))
		      (dirs (remove-if 'file-regular-p file+dir))
		      (more-files (apply #'append (mapcar 'directory-files-recursive dirs))))
		 (append files more-files)))))))

(add-to-list 'load-path "~/init/")
(add-to-list 'load-path "~/init/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'backport)
(require 'fr-mode)
(require 'storm-skim-mode)
(require 'timesync)
(require 'kafka)
(require 'storm)
