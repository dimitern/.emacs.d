;; ----------------------------------------------------------------------------
;; dimitern-customize: Setup easy customization
;; ----------------------------------------------------------------------------

;; Load and/or create empty custom file.
(let ((fn (expand-file-name "~/.emacs.d/emacs-customize.el")))
  (when (not (file-exists-p fn))
    (shell-command (concat "touch " fn)))
  (setq custom-file fn)
  (load custom-file))

(load custom-file 'noerror)

(provide 'dimitern-customize)
