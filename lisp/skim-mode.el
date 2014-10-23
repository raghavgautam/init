(defvar skim-begin-str "Testing going to start for" "String for searching start of test")
(defvar skim-end-str "Testing going to end for" "String for searching end of test")
(defvar skim-fail-str "]) FAILED" "String for searching failuers")
(defvar skim-success-str "]) SUCCESS" "String for searching success")
(defvar skim-skip-str "]) SKIPPED" "String for searching skipped")


(defun skim-next-begin-test ()
  "Takes you to the begining of the next test."
  (interactive)
  (search-forward skim-begin-str))

(defun skim-prev-begin-test ()
  "Takes you to the end of the previous test."
  (interactive)
  (search-backward skim-begin-str))

(defun skim-next-end-test ()
  "Takes you to the ending of the next test."
  (interactive)
  (search-forward skim-end-str))

(defun skim-prev-end-test ()
  "Takes you to the end of the previous test."
  (interactive)
  (search-backward skim-end-str))

(defun skim-next-failure ()
  "Takes you to the next failure."
  (interactive)
  (search-forward skim-fail-str))

(defun skim-prev-failure ()
  "Takes you to the previous failure."
  (interactive)
  (search-backward skim-fail-str))

(defun skim-next-success ()
  "Takes you to the next success."
  (interactive)
  (search-forward skim-success-str))

(defun skim-prev-success ()
  "Takes you to the previous success."
  (interactive)
  (search-backward skim-success-str))

(defun skim-next-skip ()
  "Takes you to the next skip."
  (interactive)
  (search-forward skim-skip-str))

(defun skim-prev-skip ()
  "Takes you to the previous skip."
  (interactive)
  (search-backward skim-skip-str))


(defvar skim-map (make-sparse-keymap) "skim-mode keymap")

(define-key skim-map (kbd "M-n M-b") 'skim-next-begin-test)
(define-key skim-map (kbd "M-p M-b") 'skim-prev-begin-test)
(define-key skim-map (kbd "M-n M-e") 'skim-next-end-test)
(define-key skim-map (kbd "M-p M-e") 'skim-prev-end-test)
(define-key skim-map (kbd "M-n M-s") 'skim-next-success)
(define-key skim-map (kbd "M-p M-s") 'skim-prev-success)
(define-key skim-map (kbd "M-n M-f") 'skim-next-failure)
(define-key skim-map (kbd "M-p M-f") 'skim-prev-failure)
(define-key skim-map (kbd "M-n M-k") 'skim-next-skip)
(define-key skim-map (kbd "M-p M-k") 'skim-prev-skip)

(define-minor-mode skim-mode
  "Simplifying skimming of logs."
  :lighter " SKIM"
  :keymap skim-map)

(defun skim-annotate-start-end ()
  "put fringe marker at start and end of test"
  (interactive)
  (remove-overlays)
  (let ((skim-start-endregex
	 (regexp-opt
	  '("Testing going to start for"
	    "Testing going to end for")
	  t)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward skim-start-endregex nil t)
	(let ((overlay (make-overlay (- (point) 5) (point))))
	  (overlay-put overlay 'before-string (propertize "A"
							  'display '(left-fringe left-triangle))))
	(let ((overlay (make-overlay (line-beginning-position) (line-end-position))))
	  (overlay-put overlay 'face 'hi-blue))))))

(defun skim-annotate-failed ()
  "put fringe marker on failed tests"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (regexp-opt '("]) FAILED" "]) SKIPPED")) nil t)
      (let ((overlay (make-overlay (- (point) 7) (point))))
        (overlay-put overlay 'face 'hi-yellow)
	))))

(defun skim-annotate-more ()
  "put fringe marker on failed tests"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while
	(re-search-forward
	 (regexp-opt
	  '("Request Url:"
	    "Submitting cluster:"
	    "Submitting feed:"
	    "Submitting process:"
	    )) nil t)
      (let ((overlay (make-overlay (line-beginning-position) (line-end-position))))
	(overlay-put overlay 'face 'hi-green-b)))))

(defun skim-annotate-extra ()
  "put fringe marker on failed tests"
  (interactive)
  (let ((keywords
	 '(
	   ;;associated with internal server error
	   "Invalid Workflow server or port"
	   "HTTP ERROR 500"
	    ;;associated with internal server error
	   "org.mortbay.thread.QueuedThreadPool$PoolThread.run")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (regexp-opt keywords) nil t)
	(let ((overlay (make-overlay (line-beginning-position) (line-end-position))))
	  (overlay-put overlay 'face 'hi-red-b))))))

(defun skim-annotate-all ()
  "apply all the skim annotations on the log files"
  (interactive)
  (skim-annotate-start-end)
  (skim-annotate-failed)
  (skim-annotate-more)
  (skim-annotate-extra))


(provide 'skim-mode)

