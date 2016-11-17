;; dimitern-modes.el: Per-mode configurations.
;;

(setq
 ;; Use text mode by default for new buffers.
 default-major-mode 'text-mode
 ;; Default indent 4 spaces per tab, use tabs.
 standard-indent 4
 indent-tabs-mode t
 ;; Double space after sentence, final newline.
 sentence-end-double-space nil
 require-final-newline t
 ;; Set default fill column
 fill-column 80
 ;; Miscellaneous
 auto-mode-alist (cons '("\\.mm$" . c++-mode) auto-mode-alist)
 c-default-style "linux"
 vc-follow-symlinks t
 )

;; When saving files, set execute permission if #! is in first line.
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(provide 'dimitern-modes)