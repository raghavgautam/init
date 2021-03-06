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
(ignore-errors (require 'cl-lib))
(ignore-errors (require 'cl))

(defvar-local fr-tmp-buf "fr-tmp-buf" "temp buffer for processing related to fr-mode")

(defvar-local host-name nil "host name to use for analysis")
(defvar-local ooz-bin nil "oozie binary name")
(defvar-local ooz-url nil "oozie binary name")
(defvar-local ooz-job-cmd nil "oozie job command")

(defvar-local ooz-inf-buf "oozie-info" "Oozie info buffer")
(defvar-local ooz-inf-cmd nil "oozie info command")

(defvar-local ooz-log-buf "oozie-log" "Oozie log buffer")
(defvar-local ooz-log-cmd nil)

(defvar-local cmd-buf "command" "Output of a generic command")

(defvar-local yarn-inf-buf "yarn-info" "Yarn info buffer")
(defvar-local yarn-inf-cmd nil "yarn info command")

(defvar-local setup-buf "setup" "Output of setup command")
(defcustom setup-cmd
  "curl -L https://github.com/raghavgautam/init/archive/master.zip -o /tmp/`whoami`.zip; unzip -o -d /tmp/`whoami`/ /tmp/`whoami`.zip; chmod -R 777 /tmp/`whoami`/; mkdir ~/.emacs.d/; rm -rf ~/.emacs.d/lisp; mv /tmp/`whoami`/init-master/* /tmp/`whoami`/init-master/.[^.]* ~/.emacs.d/; ~/.emacs.d/setup.sh"
  "Setup command"
  :group 'fr)
;;(setq ac-delay 0.1)
(defun cmd-weave (&rest cmd-parts)
  "Weave parts of command together"
  (mapconcat 'identity cmd-parts " "))

(defun fr-run-cmd (command buffer)
  "Run the COMMAND in BUFFER. Setup the buffer after running the command"
  (let ((host host-name))
    (shell-command command buffer)
    (switch-to-buffer buffer)
    (delete-other-windows)
    (fr-mode t)
    (fr-set-vars host)))

(defun fr-run-cmd-get-output (command)
  "Run the COMMAND get string output"
  (shell-command command fr-tmp-buf)
  (switch-to-buffer fr-tmp-buf)
  (delete-other-windows)
  (with-current-buffer fr-tmp-buf
    (let ((output (buffer-string)))
      (kill-buffer)
      output)))

(defun gethostname ()
  "get hostname for current buffer"
  (if (string= system-type "windows-nt") (replace-regexp-in-string "\n$" "" (fr-run-cmd-get-output (cmd-weave "hostname")))
    (replace-regexp-in-string "\n$" "" (fr-run-cmd-get-output (cmd-weave "hostname" "-f")))))

(defun fr-set-vars (host)
  (interactive
   (list (read-from-minibuffer "Hostname: " (gethostname))))
  (setq host-name host
	ooz-bin (or (car (file-expand-wildcards "d:/hdp/oozie-*/oozie-win-distro/bin/oozie"))
		    "oozie")
	ooz-url (concat "http://" host-name ":11000/oozie")
	ooz-job-cmd (cmd-weave ooz-bin "job" "-oozie" ooz-url)
	ooz-inf-cmd (cmd-weave ooz-job-cmd "-info")
	ooz-log-cmd (cmd-weave ooz-job-cmd "-log"))
  (message "var setting done."))

(defun fr-custom-run (command)
  (interactive
   (list (read-from-minibuffer "Command: " "ls -a")))
  (fr-run-cmd command cmd-buf))

(defun job-at-point ()
  (let ((pt (point)))
    (buffer-substring-no-properties
     (save-excursion
       (skip-chars-backward "-a-zA-Z0-9@")
       (point))
     (save-excursion
       (skip-chars-forward "-a-zA-Z0-9@")
       ;;(skip-chars-backward "." pt)
       (point)))))

(defun fr-oozie-info (command)
  (interactive
   (list (read-from-minibuffer "Command: " (cmd-weave ooz-inf-cmd (job-at-point)))))
  (fr-run-cmd command ooz-inf-buf))

(defun fr-oozie-log (command)
  (interactive
   (list (read-from-minibuffer "Command: " (cmd-weave ooz-log-cmd (job-at-point)))))
  (fr-run-cmd command ooz-log-buf))

(defun fr-setup (command)
  (interactive
   (list (read-from-minibuffer "Command: " setup-cmd)))
  (fr-run-cmd command setup-buf))

(when (bound-and-true-p laptop)
  (use-package projectile :ensure))

(defun fr-find-file-in-project (hint)
  (let ((file (projectile-completing-read
	       "Find file: "
	       (projectile-current-project-files)
	       hint)))
    (with-current-buffer (find-file (expand-file-name file (projectile-project-root)))
      (fr-mode))))

(defun fr-oozie-handler (job_id)
  (let* ((oozie-op (ido-completing-read "Do you wish info or log for this oozie job ?" (list "info" "log") nil t))
	 (command
	  (read-from-minibuffer
	   "Constructed command: "
	   (cmd-weave ooz-job-cmd (concat "-" oozie-op) job_id))))
    (fr-run-cmd command
		(if (string-equal oozie-op "log")
		    ooz-log-buf
		  ooz-inf-buf))))

(defun fr-yarn-handler (job_id)
  (let* ((command
	  (read-from-minibuffer
	   "Constructed command: "
	   (cmd-weave "yarn logs -applicationId" job_id))))
    (fr-run-cmd command yarn-inf-buf)))

(defun fr-action-handler (b)
  (let* ((content (button-label b))
	 (job_id  (replace-regexp-in-string "job" "application" content)))
    (cond ((bound-and-true-p laptop)
	   (fr-find-file-in-project (car (split-string job_id "@"))))
	  ((string-match-p "oozie" job_id)
	   (fr-oozie-handler job_id))
	  ((string-match-p "application" job_id)
	   (fr-yarn-handler job_id))
	  (t (message "Don't know how to handle %s" job_id)))))

(defun fr-requestid-action-handler (b)
  (let* ((requestid (button-label b))
	 (falcon-app-log-filename (cl-remove-if-not (lambda (x) (string-match-p "falcon.application.log" x)) (projectile-current-project-files)))
	 (project-root-dir (projectile-project-root))
	 (falcon-app-logs (mapcar (lambda (x) (concat project-root-dir x)) falcon-app-log-filename)))
    (mapcar (lambda (one-log)
	      (with-current-buffer (find-file-noselect one-log)
		(goto-char (point-min))
		(when (search-forward requestid nil t)
		  (switch-to-buffer (current-buffer)))))
	    falcon-app-logs)))

(defun add-button-for-regex (regex button)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regex nil t)
      (make-button ;;or make-button/make-text-button
       (match-beginning 0)
       (match-end 0)
       :type button))))

(define-button-type 'fr-action-button
  'action 'fr-action-handler
  'follow-link t)

(define-button-type 'fr-requestid-action-button
  'action 'fr-requestid-action-handler
  'follow-link t)

;;(fr-custom-run "find .")
(defun goto-next-link ()
  (interactive)
  (forward-button 1))

(defun goto-prev-link ()
  (interactive)
  (backward-button 1))

(defvar fr-map (make-sparse-keymap) "fr-mode keymap")
(define-key fr-map (kbd "C-c n") 'goto-next-link)
(define-key fr-map (kbd "C-c p") 'goto-prev-link)
(define-key fr-map (kbd "C-c C-n") 'goto-next-link)
(define-key fr-map (kbd "C-c C-p") 'goto-prev-link)
(define-key fr-map (kbd "C-c u") 'fr-custom-run)
(define-key fr-map (kbd "C-c i") 'fr-oozie-info)
(define-key fr-map (kbd "C-c l") 'fr-oozie-log)
(define-key fr-map (kbd "C-c s") 'fr-setup)
(define-key fr-map (kbd "C-c v") 'fr-set-vars)
(define-key fr-map (kbd "C-c d") 'load-file)
(define-key fr-map (kbd "q") (lambda () (interactive)
			       (if (s-ends-with? "-output.txt" (buffer-file-name))
				   (message "Not killing test log buffer: %s" (buffer-file-name))
				 (kill-buffer))))
;;;###autoload
(define-minor-mode fr-mode
  "Simplifying analysis of hadoop and oozie jobs."
  :lighter " FR"
  :keymap fr-map)

;;;###autoload
;;(add-hook 'text-mode-hook 'foo-mode)

;;(setq fr-mode-hook nil)
(require 'skim-mode)
;;(setq fr-mode-hook nil)
(add-hook 'fr-mode-hook 'read-only-mode)
(add-hook 'fr-mode-hook 'skim-mode)
(add-hook 'fr-mode-hook '(lambda () (jit-lock-mode t)))
(add-hook 'fr-mode-hook 'fr-decorator t)
(add-hook 'fr-mode-hook '(lambda () (fr-set-vars (gethostname))))

(defconst fr-oozie-id-regex "\\([[:digit:]]\\{7\\}-[[:digit:]]\\{15\\}-oozie-\\(oozi\\|hado\\)-[BCW]\\(@[[:digit:]]+\\)?\\|\\(job\\|application\\)_[[:digit:]]\\{13\\}_[[:digit:]]\\{4\\}\\)")
(defconst fr-screenshot-regex    "[a-z][A-Z]+\\.[a-zA-Z]+(\\[.*\\])")
(defconst fr-falcon-requestid-regex "\\([[:alnum:]]\\{8\\}-[[:alnum:]]\\{4\\}-[[:alnum:]]\\{4\\}-[[:alnum:]]\\{4\\}-[[:alnum:]]\\{12\\}\\)")

(defun fr-decorator ()
  (add-button-for-regex fr-oozie-id-regex         'fr-action-button)
  (add-button-for-regex fr-screenshot-regex       'fr-action-button)
  (add-button-for-regex fr-falcon-requestid-regex 'fr-requestid-action-button))

(provide 'fr-mode)
