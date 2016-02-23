;;find-file-at-point already exists, this is a poor man's implementation for machines where it is not available
(defun open-file-at-point ()
  "Open file at point"
  (interactive)
  (find-file (substring-no-properties (thing-at-point 'filename))))


