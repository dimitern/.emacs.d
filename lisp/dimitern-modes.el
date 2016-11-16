;; dimitern-modes.el: Per-mode configurations.
;;

;; Use text mode by default for new buffers.
(setq default-major-mode 'text-mode)
(setq initial-major-mode 'text-mode)

;; Default indent 4 spaces per tab, use tabs.
(setq standard-indent 4)
(setq-default indent-tabs-mode t)

;; Double space after sentence, final newline.
(setq sentence-end-double-space nil)
(setq require-final-newline t)

;; Set default fill column
(setq-default fill-column 80)

;; When saving files, set execute permission if #! is in first line.
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

;; Miscellaneous
(setq auto-mode-alist (cons '("\\.mm$" . c++-mode) auto-mode-alist))
(setq c-default-style "linux")
(setq vc-follow-symlinks t)

(provide 'dimitern-modes)
