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

(defvar-local setup-buf "setup" "Output of setup command")
(defvar-local setup-cmd nil "Output of setup command")

(defvar fr-begin-str "Testing going to start for" "String for searching start of test")
(defvar fr-end-str "Testing going to end for" "String for searching end of test")
(defvar fr-fail-str "]) FAILED" "String for searching failuers")
(defvar fr-success-str "]) SUCCESS" "String for searching success")
(defvar fr-skip-str "]) SKIPPED" "String for searching skipped")

;(setq ac-delay 0.1)
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
  (fr-run-cmd command fr-tmp-buf)
  (with-current-buffer fr-tmp-buf
    (let ((output (buffer-string)))
      (kill-buffer)
      output)))
  
(defun gethostname ()
  "get hostname for current buffer"
  (replace-regexp-in-string "\n$" "" (fr-run-cmd-get-output (cmd-weave "hostname" "-f"))))

(defun fr-set-vars (host)
  (interactive
   (list (read-from-minibuffer "Hostname: " (gethostname))))
  (setq setup-cmd "curl -L https://github.com/raghavgautam/init/archive/master.zip -o /tmp/`whoami`.zip; unzip -o -d /tmp/`whoami`/ /tmp/`whoami`.zip; chmod -R 777 /tmp/`whoami`/; mkdir ~/.emacs.d/; mv /tmp/`whoami`/init-master/* /tmp/`whoami`/init-master/.[^.]* ~/.emacs.d/; ~/.emacs.d/setup.sh")
  (setq host-name host
	ooz-bin "oozie"
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

(defun fr-next-begin-test ()
  "Takes you to the begining of the next test."
  (interactive)
  (search-forward fr-begin-str))

(defun fr-prev-begin-test ()
  "Takes you to the end of the previous test."
  (interactive)
  (search-backward fr-begin-str))

(defun fr-next-end-test ()
  "Takes you to the ending of the next test."
  (interactive)
  (search-forward fr-end-str))

(defun fr-prev-end-test ()
  "Takes you to the end of the previous test."
  (interactive)
  (search-backward fr-end-str))

(defun fr-next-failure ()
  "Takes you to the next failure."
  (interactive)
  (search-forward fr-fail-str))

(defun fr-prev-failure ()
  "Takes you to the previous failure."
  (interactive)
  (search-backward fr-fail-str))

(defun fr-next-success ()
  "Takes you to the next success."
  (interactive)
  (search-forward fr-success-str))

(defun fr-prev-success ()
  "Takes you to the previous success."
  (interactive)
  (search-backward fr-success-str))

(defun fr-next-skip ()
  "Takes you to the next skip."
  (interactive)
  (search-forward fr-skip-str))

(defun fr-prev-skip ()
  "Takes you to the previous skip."
  (interactive)
  (search-backward fr-skip-str))

;;(fr-custom-run "find .")

(defvar fr-map (make-sparse-keymap) "fr-mode keymap")
(define-key fr-map (kbd "C-c n") nil)
(define-key fr-map (kbd "C-c p") nil)
(define-key fr-map (kbd "C-c n b") 'fr-next-begin-test)
(define-key fr-map (kbd "C-c p b") 'fr-prev-begin-test)
(define-key fr-map (kbd "C-c n e") 'fr-next-end-test)
(define-key fr-map (kbd "C-c p e") 'fr-prev-end-test)
(define-key fr-map (kbd "C-c n s") 'fr-next-success)
(define-key fr-map (kbd "C-c p s") 'fr-prev-success)
(define-key fr-map (kbd "C-c n f") 'fr-next-failure)
(define-key fr-map (kbd "C-c p f") 'fr-prev-failure)
(define-key fr-map (kbd "C-c n k") 'fr-next-skip)
(define-key fr-map (kbd "C-c p k") 'fr-prev-skip)
(define-key fr-map (kbd "C-c u") 'fr-custom-run)
(define-key fr-map (kbd "C-c i") 'fr-oozie-info)
(define-key fr-map (kbd "C-c l") 'fr-oozie-log)
(define-key fr-map (kbd "C-c s") 'fr-setup)
(define-key fr-map (kbd "C-c v") 'fr-set-vars)
(define-key fr-map (kbd "C-c d") 'load-file)
;;;###autoload
(define-minor-mode fr-mode
  "Simplifying analysis of hadoop and oozie jobs."
  :lighter " FR"
  :keymap fr-map)

;;;###autoload
;;(add-hook 'text-mode-hook 'foo-mode)

;;(setq fr-mode-hook nil)
;;(add-hook 'fr-mode-hook '(lambda () (fr-set-vars (gethostname))))

(provide 'fr-mode)