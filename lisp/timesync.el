(defconst bogus-date (date-to-time ""))  ;;(14445 17280) is bogus


(defun date-of-log (log-line)
  (let* ((dt-part (substring log-line 0 (min 30 (length log-line)))))
    (date-to-time dt-part)))

(defun buffer-time-sync (&optional buff)
  "Time sync current buffer with another one" 
  (interactive "bBuffer to sync with? ")
  (let ((search-date (with-current-buffer buff
		       (date-of-log (thing-at-point 'line)))))
    (tsearch search-date)))

(defalias 'time-sync-buffer 'buffer-time-sync)
(defalias 'timesync-buffer 'buffer-time-sync)

(defun tsearch (search-time)
  "Search for time `search-time' in the current buffer"
  (goto-char (point-min))
  (custom-binary-search-impl
   nil
   search-time 
   (lambda (ignore line-num)
     (goto-char (point-min))
     (forward-line (1- line-num))
     (date-of-log (thing-at-point 'line)))
   'time-less-p
   0
   (1+ (count-lines (point-min) (point-max)))
   ))


(defun custom-binary-search-impl (ds elem afun lessp left right)
  (let* ((mid (/ (+ left right) 2))
	 (mid-val (funcall afun ds mid)))
    (while (equal mid-val bogus-date)
      (forward-line -1)
      (setq
       mid (1- mid))
      (unless (looking-at-p "    ")
	(setq
	 mid-val (funcall afun ds mid))))
    ;;(message (format "using mid: %d mid-val: %s"  mid (format-time-string "%T %D" mid-val)))
    (cond ((> left right) right)
	  ((equal elem mid-val) mid)
	  ((not (funcall lessp elem mid-val))
	   (custom-binary-search-impl ds elem afun lessp (+ mid 1) right))
	  ((funcall lessp elem mid-val)
	   (custom-binary-search-impl ds elem afun lessp left (- mid 1))))))

(provide 'timesync)

