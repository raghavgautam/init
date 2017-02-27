(require 'lib)
(defun storm.yaml.file.get (&optional prefix)
  "Get storm.yaml.file optionally setting it. Use PREFIX arg force setting it."
  (interactive "P")
  (when (or prefix (unbound-p storm.yaml.file))
    (setq storm.yaml.file
          (let ((default-directory "/etc/storm"))
            (read-file-name "Enter path for storm.yaml file:"))))
  storm.yaml.file)
;;(storm.yaml.file.get)

(defun storm.secure-p ()
  "Find if storm is running in secure setup."
  (let ((jaas_conf (ignore-errors (find-in-file (storm.yaml.file.get) "java.security.auth.login.config *: *'\\([^']+\\)'" 1))))
    (when jaas_conf
      (string-match-p "storm_jaas.conf *$" jaas_conf))))
;;(storm.secure-p)


;;/usr/hdp/current/storm-client/bin/storm -c java.security.auth.login.config=/etc/storm/conf/client_jaas.conf -c storm.thrift.transport=org.apache.storm.security.auth.kerberos.KerberosSaslTransportPlugin -c client.jartransformer.class=nil list
(defun storm.list ()
  "List storm topologies."  
  (let* ((storm-bin "storm")
	 (command (concat
		   storm-bin
		   (when (storm.secure-p) " -c java.security.auth.login.config=/etc/storm/conf/client_jaas.conf -c storm.thrift.transport=org.apache.storm.security.auth.kerberos.KerberosSaslTransportPlugin")
		   " -c client.jartransformer.class=nil"
		   " list")))
    command))
;;(storm.list)
