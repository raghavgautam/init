(defvar storm-skim-begin-str "Testing going to start for" "String for searching start of test")
(defvar storm-skim-end-str "Testing going to end for" "String for searching end of test")
(defvar storm-test-start-regex "\\(20[1-5][0-9]-[01][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9],[0-9][0-9][0-9].*?RUNNING TEST .*? at location .*? at line number .*?\\|[tT]est [cC]ase [nN]ame : testcase_\\)")
(setq storm-test-end-regex "\\(20[1-5][0-9]-[01][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9],[0-9][0-9][0-9].*?TEST .*? in .*? seconds\\)\\|\\(Testing going to end for: .*? ----- Status: .+?\\)")
(setq storm-test-fail-regex "\\(20[1-5][0-9]-[01][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9],[0-9][0-9][0-9].*?TEST .*? FAILED in .*? seconds\\|tests/storm/.*?\\.py:[0-9]+?: .*\\|Error: .* [fF]ailed\\|Testing going to end for: .*? ----- Status: FAILED\\)")
(defvar skim-success-str "]) SUCCESS" "String for searching success")
(defvar skim-success-str2 "]) ----- Status: SUCCESS" "String for searching success")
(defvar skim-skip-str "]) SKIPPED" "String for searching skipped")
(defvar skim-skip-str2 "]) ----- Status: SKIPPED" "String for searching skipped")
(defvar skim-request-str "Request Url: " "String for determining falcon request")
(defvar skim-record-regex "\\(?:20[1-5][0-9]-[01][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9],[0-9][0-9][0-9]\\)" "Regex for determining begining of a log record")
(defvar skim-important-logs-regex
  "\\(?:20[1-5][0-9]-[01][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9],[0-9][0-9][0-9]\\).*?[*]\\{3,20\\}[ _a-zA-Z]\\{3,50\\}[*]\\{3,20\\}"
  "Regex for important log messages.")
(defvar skim-odd-record-regex "Traceback (most recent call last)\\|Exception" "Regex for determining odd stuff.")

(defvar storm-skim-map (make-sparse-keymap) "skim-mode keymap")


(defun mad-button-kafka-action (button)
  (interactive)
  (let* ((hint (button-get button 'search-hint))
	 ;;(all-properties (text-properties-at (button-start button)))
	 ;;(build-url (get-text-property (button-start button) 'build-url))
	 (file (projectile-completing-read
		"Find file: "
		(projectile-current-project-files)
		hint)))
    (find-file (expand-file-name file (projectile-project-root)))))

(defun mad-button-kafka-echo (window object position)
  (concat "Open file for: " (button-get (button-at position) 'full-text)))

(define-button-type 'mad-button-kafka
  'follow-link t
  'action 'mad-button-kafka-action
  'help-echo 'mad-button-kafka-echo)

(defun mad-link-kafka ()
  (interactive)
  (save-excursion
    (goto-char 200)
    (remove-overlays)
    (while (re-search-forward "/usr/hdp/current/kafka\\-broker/system_test/\\([^ '\n]*\\)" nil t)
      (let* ((start (match-beginning 0))
	     (end (match-end 0))
	     (text (match-string-no-properties 1))
	     (full-text (match-string-no-properties 0))
	     (ov (make-overlay start end)))
	;;(overlay-put ov 'display (concat ":system_test" text))
	;;(overlay-put ov )
	(make-button start end
		     :type 'mad-button-kafka
		     'follow-link t
		     ;;'face nil
		     'search-hint text
		     'full-text full-text)))))

(defun add-regex-search (function-name-suffix doc-template regex &optional kbd-postfix)
  (let ((next-function (intern (format "storm-skim-next-%s" function-name-suffix)))
	(next-doc (format doc-template "next"))
	(next-key (concat "M-n M-" kbd-postfix))
	(prev-function (intern (format "storm-skim-prev-%s" function-name-suffix)))
	(prev-doc (format doc-template "prev"))
	(prev-key (concat "M-p M-" kbd-postfix)))
    (fset next-function
	  `(lambda ()
	     ,next-doc
	     (interactive)
	     (re-search-forward ,regex)))
    (when kbd-postfix
      (define-key storm-skim-map (kbd next-key) next-function))
    (fset prev-function
	  `(lambda ()
	     ,prev-doc
	     (interactive)
	     (re-search-backward ,regex)))
    (when kbd-postfix
      (define-key storm-skim-map (kbd prev-key) prev-function))))

(add-regex-search 'begin-test "Go to begining of the %s test." storm-test-start-regex "b")
(add-regex-search 'end-test   "Go to ending of the %s test."   storm-test-end-regex "e")
(add-regex-search 'failure    "Go to %s failure." storm-test-fail-regex "f")
(add-regex-search 'success    "Go to %s success."  (regexp-opt (list skim-success-str skim-success-str2)) "s")
(add-regex-search 'request    "Go to %s request."  skim-request-str "r")
(add-regex-search 'record     "Go to %s record" skim-record-regex "g")
(add-regex-search 'record     "Go to %s odd record" skim-odd-record-regex "o")

(defun skim-annotate-failed ()
  "put fringe marker on failed tests"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (regexp-opt '("]) FAILED" "]) SKIPPED")) nil t)
      (let ((overlay (make-overlay (- (point) 7) (point))))
        (overlay-put overlay 'face 'hi-yellow)
	))))

(defun storm-skim-annotate-more ()
  "put fringe marker on failed tests"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while
	(re-search-forward skim-important-logs-regex nil t)
      (let ((overlay (make-overlay (line-beginning-position) (line-end-position))))
	(overlay-put overlay 'face 'hi-green-b)))))

(defun storm-skim-annotate-extra ()
  "put fringe marker on failed tests"
  (interactive)
  (let ((keywords
	 '(
	   ;;associated with internal server error
	   "Invalid Workflow server or port"
	   "HTTP ERROR 500"
	   "Exception"
	   "exception"
	   "storm/bin/"
	   "kafka/bin/"
	   "storm\\bin\\"
	   "kafka\\bin\\"
	   "curl"
	   ;;associated with internal server error
	   "org.mortbay.thread.QueuedThreadPool$PoolThread.run")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (regexp-opt keywords) nil t)
	(let ((overlay (make-overlay (line-beginning-position) (line-end-position)))
	      (overlay2 (make-overlay (match-beginning 0) (match-end 0))))
	  (overlay-put overlay 'face 'hi-blue)
	  (overlay-put overlay2 'face 'hi-blue-b))))))

(defun storm-skim-test-start-end-annotate ()
  "Storm specific annotations."
  (interactive)
  (save-excursion
    (-each
	(list storm-test-start-regex storm-test-end-regex)
      (lambda (x)
	(goto-char (point-min))
	(while (re-search-forward x nil t)
	  (let ((overlay (make-overlay (- (point) 5) (point))))
	    (overlay-put overlay 'before-string (propertize "A"
							    'display '(left-fringe left-triangle))))
	  (let ((overlay (make-overlay (line-beginning-position) (line-end-position))))
	    (overlay-put overlay 'face 'hi-green)))))))

(defun storm-skim-annotate-all ()
  "apply all the skim annotations on the log files"
  (interactive)
  (require 'hi-lock)
  (storm-skim-annotate-extra)
  (storm-skim-annotate-more)
  (storm-skim-test-start-end-annotate)
  (skim-annotate-failed))

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
    (re-search-backward storm-skim-begin-str)
    (move-beginning-of-line nil)
    (transient-mark-mode 1)
    (set-mark (point))
    (re-search-forward storm-skim-end-str)
    (move-beginning-of-line nil)
    (forward-line)))

(define-key storm-skim-map (kbd "M-n M-l") 'goto-next-link)
(define-key storm-skim-map (kbd "M-p M-l") 'goto-prev-link)
(define-key storm-skim-map (kbd "TAB") 'storm-skim-next-failure)
(define-key storm-skim-map (kbd "<backtab>") 'storm-skim-prev-failure)

(define-minor-mode storm-skim-mode
  "Simplifying skimming of logs."
  :lighter " StSk"
  :keymap storm-skim-map)

(setq storm-skim-font-lock-keywords
      `(
        ;;(,mylsl-type-regexp . font-lock-type-face)
        (,storm-test-start-regex . font-lock-constant-face)
	(,storm-test-end-regex . font-lock-constant-face)
        ;;(,mylsl-event-regexp . font-lock-builtin-face)
        ;;(,mylsl-functions-regexp . font-lock-function-name-face)
        ;;(,mylsl-keywords-regexp . font-lock-keyword-face)
        ))
;;define-minor-mode
;;http://emacs.stackexchange.com/questions/10165/can-i-add-highlighting-in-a-minor-mode
(define-derived-mode ead-mode nil " EAD"
  "Emacs assisted debugging."
  ;;(use-local-map storm-skim-map)
  (setq font-lock-defaults '((storm-skim-font-lock-keywords))
	ead-mode-map storm-skim-map))

(provide 'storm-skim-mode)
;;; storm-skim-mode.el ends here
