(defconst bogus-date (date-to-time ""))  ;;(14445 17280) is bogus

;;(benchmark-run-compiled 40000 (date-to-time "2014-10-12 11:11:11"))

(defun date-of-log (log-line)
  (let* ((dt-part (substring log-line 0 (min 30 (length log-line)))))
    (ignore-errors (let ((date (date-to-time dt-part)))
		     (if (equal date bogus-date) nil
		       date)))))

(defun buffer-timesync (&optional buff)
  "Time sync current buffer with another one" 
  (interactive "bBuffer to sync with? ")
  (let ((search-date
	 (with-current-buffer buff
	   (save-excursion
	     (let ((date (date-of-log (thing-at-point 'line))))
	       (while (not date)
		 (forward-line -1)
		 (setq date (date-of-log (thing-at-point 'line))))
	       date)))))
    (tsearch search-date)))

(defalias 'timesync-buffer 'buffer-timesync)
(defalias 'timesync-buffer 'buffer-timesync)

(defun timesync-log-date ()
  (let* ((line (thing-at-point 'line))
	 (date (date-of-log line)))
    date))

(defun tsearch (search-time)
  "Search for time `search-time' in the current buffer"
  (goto-char (point-min))
  (custom-binary-search-impl
   nil
   search-time 
   (lambda (buff line-num)
     (goto-char (point-min))
     (forward-line (1- line-num))
     (timesync-log-date))
   'time-less-p
   0
   (1+ (count-lines (point-min) (point-max)))))

(defcustom timesync-debug nil
  "Switch for timesync debugging"
  :type 'boolean
  :group 'timesync
  :version "24.3")

(setq timesync-debug t)

;;this is differnt from standard binary search
(defun custom-binary-search-impl (ds elem afun lessp left right &optional prev-gap)
  (when (bound-and-true-p timesync-defug) (message "Time Window: %s %s" left right))
  (let ((curr-gap (- right left)))
    (if (and prev-gap (= prev-gap curr-gap)) ;;ensuring convergence
	(progn (when (bound-and-true-p timesync-debug)
		 (message "Converged: %s %s" left right))
	       ;;TODO: drop to linear search
	       (when (= 1 prev-gap)
		 (error "Implement linear search: %s %s" left right)))

      (let* ((mid (/ (+ left right) 2))
	     (mid-val (ignore-errors (funcall afun ds mid))))
	(while (not mid-val)
	  (forward-line -1)
	  (setq mid (1- mid))
	  (unless (looking-at-p "\\(    \\|\t\\)")
	    (setq
	     mid-val (funcall afun ds mid))))
	;;(message (format "using mid: %d mid-val: %s"  mid (format-time-string "%T %D" mid-val)))
	(cond ;;((> left right) right)
	 ;;((equal elem mid-val) mid)
	 ((not (funcall lessp elem mid-val))
	  (custom-binary-search-impl ds elem afun lessp (+ mid 1) right curr-gap))
	 ((funcall lessp elem mid-val)
	  (custom-binary-search-impl ds elem afun lessp left (- mid 1) curr-gap)))))))

(provide 'timesync)

