(defvar skim-begin-str "Testing going to start for" "String for searching start of test")
(defvar skim-end-str "Testing going to end for" "String for searching end of test")
(defvar skim-fail-str "]) FAILED" "String for searching failuers")
(defvar skim-success-str "]) SUCCESS" "String for searching success")
(defvar skim-skip-str "]) SKIPPED" "String for searching skipped")
(defvar skim-request-str "Request Url: " "String for determining falcon request")
(defvar skim-record-regex "\\(?:20[1-5][0-9]-[01][0-9]-[0-3][0-9] [0-1][0-9]:[0-5][0-9]:[0-5][0-9],[0-9][0-9][0-9]\\)" "Regex for determining begining of a log record")


(defvar skim-map (make-sparse-keymap) "skim-mode keymap")

(defun add-regex-search (function-name-suffix doc-template regex &optional kbd-postfix)
  (let ((next-function (intern (format "skim-next-%s" function-name-suffix)))
	(next-doc (format doc-template "next"))
	(next-key (concat "M-n M-" kbd-postfix))
	(prev-function (intern (format "skim-prev-%s" function-name-suffix)))
	(prev-doc (format doc-template "prev"))
	(prev-key (concat "M-p M-" kbd-postfix)))
    (fset next-function
	  `(lambda ()
	     ,next-doc
	     (interactive)
	     (re-search-forward ,regex)))
    (when kbd-postfix
      (define-key skim-map (kbd next-key) next-function))
    (fset prev-function
	  `(lambda ()
	     ,prev-doc
	     (interactive)
	     (re-search-backward ,regex)))
    (when kbd-postfix
      (define-key skim-map (kbd prev-key) prev-function))))

(add-regex-search 'begin-test "Go to begining of the %s test." skim-begin-str "b")
(add-regex-search 'end-test   "Go to ending of the %s test."   skim-end-str   "e")
(add-regex-search 'failure    "Go to %s failure." (regexp-opt (list skim-fail-str skim-skip-str)) "f")
(add-regex-search 'success    "Go to %s success."  skim-success-str "s")
(add-regex-search 'request    "Go to %s request."  skim-request-str "r")
(add-regex-search 'record     "Go to %s record" skim-record-regex "g")

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
  (require 'hi-lock)
  (skim-annotate-start-end)
  (skim-annotate-failed)
  (skim-annotate-more)
  (skim-annotate-extra))

(setq skim-record-regex
      "\\(?:20[1-5][0-9]-[01][0-9]-[0-3][0-9] [0-1][0-9]:[0-5][0-9]:[0-5][0-9],[0-9][0-9][0-9]\\)")

(defun skim-select-record ()
  "put fringe marker on failed tests"
  (interactive)
  (progn
    (re-search-backward skim-record-regex)
    (transient-mark-mode 1)                                                                                                                                                        
    (set-mark (point))                                                                                                                                                             
    (re-search-forward skim-record-regex)
    (re-search-forward skim-record-regex)
    (move-beginning-of-line nil)
    ;;(print (concat "test " (number-to-string (mark)) " " (number-to-string (point))))
    ))

(defun skim-select-test ()
  "put fringe marker on failed tests"
  (interactive)
  (progn
    (re-search-backward skim-begin-str)
    (move-beginning-of-line nil)
    (transient-mark-mode 1)                                                                                                                                                        
    (set-mark (point))                                                                                                                                                             
    (re-search-forward skim-end-str)
    (move-beginning-of-line nil)
    (forward-line)
    ))

(define-key skim-map (kbd "M-n M-l") 'goto-next-link)
(define-key skim-map (kbd "M-p M-l") 'goto-prev-link)
(define-key skim-map (kbd "TAB") 'skim-next-failure)
(define-key skim-map (kbd "<backtab>") 'skim-prev-failure)

(define-minor-mode skim-mode
  "Simplifying skimming of logs."
  :lighter " SKIM"
  :keymap skim-map)

(provide 'skim-mode)

