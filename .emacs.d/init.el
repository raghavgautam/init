(setq ido-enable-flex-matching t
      ido-everywhere t)
(ido-mode 1)
(put 'narrow-to-region 'disabled nil)
(setq auto-mode-alist (cons '("\\.txt$" . auto-revert-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.log$" . auto-revert-tail-mode) auto-mode-alist))
(setq dired-auto-revert-buffer t)
