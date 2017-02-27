(require 'lib)
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
    (setq kafka.broker (find-in-file (kafka.server.props.file.get prefix) "listeners *=.*://\\([^:]+:[0-9]+\\)" 1))))
;;(kafka.broker.get nil)

(defun kafka.server.props.file.get (&optional prefix)
  "Get kafka.server.props.file optionally setting it. Use PREFIX arg force setting it."
  (interactive "P")
  (when (or prefix (unbound-p kafka.server.props.file))
    (setq kafka.server.props.file
          (let ((default-directory "/etc/kafka"))
            (read-file-name "Enter location of kafka's server.properties file:"))))
  kafka.server.props.file)
;;(kafka.server.props.file.get)

(defun kafka.bin.dir.get (&optional prefix)
  "Get kafka.bin.dir optionally setting it. Use PREFIX arg force setting it."
  (interactive "P")
  (when (or prefix (unbound-p kafka.bin.dir))
    (setq kafka.bin.dir
          (let ((default-directory "/usr/hdp/current/kafka-broker/bin"))
            (read-directory-name "Enter directory for kafka binaries:"))))
  kafka.bin.dir)
;;(kafka.bin.dir.get)
(defun kafka.broker.get (&optional prefix)
  "Get kafka broker, with PREFIX reset cache."
  (interactive "P")
  (when (or prefix (unbound-p kafka.broker))
    (setq kafka.broker (find-in-file (kafka.server.props.file.get prefix) "listeners *=.*?://\\([^:]+:[0-9]+\\)" 1)))
  kafka.broker)
;;(kafka.broker.get t)

(defun kafka.security.protocol.get (&optional prefix)
  "Get kafka security protocol."
  (interactive "P")
  (find-in-file (kafka.server.props.file.get prefix) "listeners *= *\\([^:]+\\):" 1))
;;(kafka.security.protocol.get)

(defun kafka.console.consumer ()
  "Run kafka console consumer command with right args."
  (interactive)
  (let* ((topic (or (thing-at-point 'symbol)
                    (read-string "Name of the topic for console consumer: ")))
	 ;;./kafka-console-consumer.sh --bootstrap-server node-000002.hwx.site:6667 --topic topic-abc --new-consumer --security-protocol SASL_PLAINTEXT --timeout-ms 2000 --from-beginning
         (command
          (read-string "Run command: "
                       (concat (expand-file-name "kafka-console-consumer.sh" (kafka.bin.dir.get)) " --bootstrap-server " (kafka.broker.get) " --topic " topic " --new-consumer --security-protocol " (kafka.security.protocol.get) " --from-beginning --timeout-ms 2000 | head")
                       'kafka.console.consumer.history)))
    (compile-in-buffer command (concat "*kafka.console.consumer " topic "*"))))
;;(kafka.console.consumer)

(defun kafka.zookeeper.get (&optional prefix)
  "Get zookeeper endpoint; with PREFIX reset cache."
  (interactive "P")
  (when (or prefix (unbound-p kafka.zookeeper))
    (setq kafka.zookeeper (find-in-file (kafka.server.props.file.get prefix) "zookeeper.connect *= *\\(.+\\)" 1)))
  kafka.zookeeper)
;;(kafka.zookeeper.get nil)

(defun kafka.topic.list ()
  "List kafka topics"
  (interactive)
  (let* ((command
          (read-string "Run command: "
                       (concat (expand-file-name "kafka-topics.sh" (kafka.bin.dir.get)) " --zookeeper " (kafka.zookeeper.get) " --list ")
                       'kafka.topic.list.history)))
    (compile-in-buffer command "*kafka.topic.list*")))
;;(kafka.topic.list)

(defun kafka.topic.create ()
  "Create a kafka topic."
  (interactive)
  (let* ((topic (or (thing-at-point 'symbol)
                    (read-string "Name of the topic to create: ")))
         (command
          (read-string "Run command: "
                       (concat (expand-file-name "kafka-topics.sh" (kafka.bin.dir.get)) " --zookeeper " (kafka.zookeeper.get) " --create --topic " topic " --replication-factor 1 --partitions 5")
                       'kafka.topic.create.history)))
    (compile-in-buffer command "*kafka.topic.create*")))
;;(kafka.topic.create)
(provide 'kafka)
