(defun kafka.server.props.file.get (&optional prefix)
  "Get kafka.server.props.file optionally setting it. Use PREFIX arg force setting it."
  (interactive "P")
  (when (or prefix (unbound-p kafka.server.props.file))
    (setq kafka.server.props.file
          (let ((default-directory "/etc/kafka"))
            (read-file-name "Enter file name:"))))
  kafka.server.props.file)
;;(kafka.server.props.file.get)

(defun kafka.broker.get (&optional prefix)
  "Get kafka broker, with PREFIX reset cache."
  (interactive "P")
  (let ((r "\\(?:a\\)\\(BC\\)\\(?:d\\)")
        (s "_aBCd_")
        (pos 1))
    (string-match r s)
    (match-string pos s))
  (when (or prefix (unbound-p kafka.broker))
    (setq kafka.broker (find-in-file (kafka.server.props.file.get nil) "listeners *=.*://\\([^:]+:[0-9]+\\)" 1))))
;;(kafka.broker.get nil)

(defun kafka.server.props.file.get (&optional prefix)
  "Get kafka.server.props.file optionally setting it. Use PREFIX arg force setting it."
  (interactive "P")
  (when (or prefix (unbound-p kafka.server.props.file))
    (setq kafka.server.props.file
          (let ((default-directory "/etc/kafka"))
            (read-file-name "Enter file name:"))))
  kafka.server.props.file)
;;(kafka.server.props.file.get)

(defun kafka.bin.dir.get (&optional prefix)
  "Get kafka.bin.dir optionally setting it. Use PREFIX arg force setting it."
  (interactive "P")
  (when (or prefix (unbound-p kafka.bin.dir))
    (setq kafka.bin.dir
          (let ((default-directory "/usr/hdp/current/kafka-broker/bin"))
            (read-directory-name "Enter file name:"))))
  kafka.bin.dir)
;;(kafka.bin.dir.get)
(defun kafka.broker.get (&optional prefix)
  "Get kafka broker, with PREFIX reset cache."
  (interactive "P")
  (let ((r "\\(?:a\\)\\(BC\\)\\(?:d\\)")
        (s "_aBCd_")
        (pos 1))
    (string-match r s)
    (match-string pos s))
  (when (or prefix (unbound-p kafka.broker))
    (setq kafka.broker (find-in-file (kafka.server.props.file.get nil) "listeners *=.*://\\([^:]+:[0-9]+\\)" 1)))
  kafka.broker)
;;(kafka.broker.get nil)

(defun kafka.console.consumer ()
  "Run kafka console consumer command with right args."
  (interactive)
  (let* ((topic (or (thing-at-point 'symbol)
                    (read-string "Name of the topic for console consumer: ")))
         (command
          (read-string "Run command: "
                       (concat (expand-file-name "kafka-console-consumer.sh" (kafka.bin.dir.get)) " --bootstrap-server " (kafka.broker.get) " --topic " topic " --from-beginning --timeout-ms 2000 | head")
                       'kafka.console.consumer.history))
         (compilation-ask-about-save nil)
         (compilation-buffer-name-function (lambda (ignore) (concat "*kafka.console.consumer" topic "*"))))
    (compile command)))
;;(kafka.console.consumer)

(provide 'kafka)
