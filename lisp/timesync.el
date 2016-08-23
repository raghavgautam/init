(defconst bogus-date (or (ignore-errors (date-to-time "")) '(14445 17280))) ;;(14445 17280) is bogus

;;(benchmark-run-compiled 40000 (date-to-time "2014-10-12 11:11:11"))

(defun date-of-log (&optional log-line)
  (interactive)
  (unless log-line
    (setq log-line (thing-at-point 'line)))
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

(defun binary-search-pos (arr item)
  "Search sorted array for item & return closest position."
  (cond
   ((not (arrayp arr))
    (error (format "Supplied array is not an array")))
   ((<= (length arr) 0)
    (error (format "Supplied array has no element")))
   (t (binary-search-pos-1 arr item 0 (- (length arr) 1)))))

(defun binary-search-pos-1 (arr item start end)
  "Search sorted array for item & return closest position."
  (cond
   ((< item (aref arr start)) start)
   ((> item (aref arr end)) end)
   ((= start end) start)
   (t (let* ((mid (/ (+ start end) 2))
	     (elem (aref arr mid)))
	(cond
	 ((= item elem) mid)
	 ((< item elem)
	  (binary-search-pos-1 arr item start (1- mid)))
	 (t (binary-search-pos-1 arr item (1+ mid) end)))))))

(when after-init-time
  (ert-deftest binary-search-pos-tests ()
    (should-error (binary-search-pos [] 1) :type 'error)
    (should-error (binary-search-pos nil 1) :type 'error)
    (should (equal (binary-search-pos [10 20 30 40 50] 00) 0))
    (should (equal (binary-search-pos [10 20 30 40 50] 10) 0))
    (should (equal (binary-search-pos [10 20 30 40 50] 20) 1))
    (should (equal (binary-search-pos [10 20 30 40 50] 30) 2))
    (should (equal (binary-search-pos [10 20 30 40 50] 40) 3))
    (should (equal (binary-search-pos [10 20 30 40 50] 50) 4))
    (should (equal (binary-search-pos [10 20 30 40 50] 60) 4)))
  (ert-deftest date-of-log-test ()
    (should (equal (date-of-log (concat "[2016-06-29 09:08:02,358] INFO Audit Status Log: name=kafka.async.summary.multi_dest.batch.hdfs, interval=01:00.005 minutes, events=24, succcessCount=24, totalEvents=404, totalSuccessCount=404 (org.apache.ranger.audit.provider.BaseAuditHandler)\n"
					"  PID TTY          TIME CMD\n"
					" 1737 ?        00:00:02 python2.6"))
		   '(22387 62050)))
    (should (equal (date-of-log (concat "[2016-06-29 09:08:02,359] INFO Audit Status Log: name=kafka.async.summary.multi_dest.batch.hdfs, interval=01:00.005 minutes, events=24, succcessCount=24, totalEvents=404, totalSuccessCount=404 (org.apache.ranger.audit.provider.BaseAuditHandler)\n"
					"  PID TTY          TIME CMD\n"
					" 1737 ?        00:00:02 python2.6"))
		   '(22387 62050)))
    (should (equal (date-of-log (concat "[2016-06-29 09:08:12,358] INFO Audit Status Log: name=kafka.async.summary.multi_dest.batch.hdfs, interval=01:00.005 minutes, events=24, succcessCount=24, totalEvents=404, totalSuccessCount=404 (org.apache.ranger.audit.provider.BaseAuditHandler)\n"
					"  PID TTY          TIME CMD\n"
					" 1737 ?        00:00:02 python2.6"))
		   '(22387 62060))))
  (ert-deftest binary-search-log-test-regex ()
    (let ((regex (concat
		  "^"
		  "[[:digit:]]\\{4\\}.[[:digit:]]\\{2\\}.[[:digit:]]\\{2\\}" ;; year month day
		  "."
		  "[[:digit:]]\\{2\\}.[[:digit:]]\\{2\\}.[[:digit:]]\\{2\\}" ;; hour minute second
		  "."
		  "[[:digit:]]\\{3\\}" ;; milli-second
		  )))
      (should (string-match-p regex "2016-06-29 09:08:12,348"))
      (should (not (string-match-p regex "non016-06-29 09:08:12,348")))
      ))
  (ert "binary-search-*\\|timesync*\\|date-of-log*" ))

(provide 'timesync)

