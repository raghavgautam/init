(defvar skim-begin-str "Testing going to start for" "String for searching start of test")
(defvar skim-end-str "Testing going to end for" "String for searching end of test")
(defvar skim-fail-str "]) FAILED" "String for searching failuers")
(defvar skim-success-str "]) SUCCESS" "String for searching success")
(defvar skim-skip-str "]) SKIPPED" "String for searching skipped")
(defvar skim-request-str "Request Url: " "String for determining falcon request")

(defvar skim-map (make-sparse-keymap) "skim-mode keymap")
;;(define-key skim-map (kbd "M-n M-l") 'goto-next-link)

(defmacro add-regex-search (function-name-suffix doc-template regex &optional kbd-postfix)
  (let ((next-function (intern (format "skim-next-%s" function-name-suffix)))
	(next-doc (format doc-template "next"))
	(next-key (concat "M-n M-" kbd-postfix))
	(prev-function (intern (format "skim-prev-%s" function-name-suffix)))
	(prev-doc (format doc-template "prev"))
	(prev-key (concat "M-n M-" kbd-postfix)))
    `(defun ,next-function ()
       ,next-doc
       (interactive)
       (search-forward ,regex))
    (when kbd-postfix
      `(define-key skim-map (kbd ,next-key) ',next-function))

    `(defun ,prev-function ()
       ,prev-doc
       (interactive)
       (search-forward ,regex))
    (when kbd-postfix
      `(define-key skim-map (kbd ,prev-key) ',prev-function))))

(add-regex-search begin-test "Takes you to the begining of the %s test." skim-begin-str "b")
(add-regex-search end-test   "Takes you to the ending of the %s test."   skim-end-str   "e")

(defun skim-next-failure ()
  "Takes you to the next failure."
  (interactive)
  (re-search-forward (regexp-opt (list skim-fail-str skim-skip-str))))

(defun skim-prev-failure ()
  "Takes you to the previous failure."
  (interactive)
  (re-search-backward (regexp-opt (list skim-fail-str skim-skip-str))))

(defun skim-next-success ()
  "Takes you to the next success."
  (interactive)
  (search-forward skim-success-str))

(defun skim-prev-success ()
  "Takes you to the previous success."
  (interactive)
  (search-backward skim-success-str))

(defun skim-next-request ()
  "Takes you to the next request."
  (interactive)
  (search-forward skim-request-str))

(defun skim-prev-request ()
  "Takes you to the previous request."
  (interactive)
  (search-backward skim-request-str))

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

(defun skim-prev-record ()
  "go to next record"
  (interactive)
  (re-search-backward skim-record-regex))

(defun skim-next-record ()
  "go to previous log record"
  (interactive)
  (re-search-forward skim-record-regex nil t 2)
  (move-beginning-of-line nil))

(define-key skim-map (kbd "M-n M-l") 'goto-next-link)
(define-key skim-map (kbd "M-p M-l") 'goto-prev-link)
(define-key skim-map (kbd "M-n M-s") 'skim-next-success)
(define-key skim-map (kbd "M-p M-s") 'skim-prev-success)
(define-key skim-map (kbd "M-n M-r") 'skim-next-request)
(define-key skim-map (kbd "M-p M-r") 'skim-prev-request)
(define-key skim-map (kbd "M-n M-f") 'skim-next-failure)
(define-key skim-map (kbd "TAB") 'skim-next-failure)
(define-key skim-map (kbd "M-p M-f") 'skim-prev-failure)
(define-key skim-map (kbd "<backtab>") 'skim-prev-failure)


(define-key skim-map (kbd "M-n M-r") 'skim-next-record)
(define-key skim-map (kbd "M-p M-r") 'skim-prev-record)


(define-minor-mode skim-mode
  "Simplifying skimming of logs."
  :lighter " SKIM"
  :keymap skim-map)

(provide 'skim-mode)

