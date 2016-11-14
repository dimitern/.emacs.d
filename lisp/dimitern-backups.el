;; ----------------------------------------------------------------------------
;; dimitern-backups: Setup autosaves and backup files creation
;; ----------------------------------------------------------------------------

;; Store all backup and autosave files in the /tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq savehist-mode t)
(setq history-lenth 1000)

;; Share the clipboard
(setq x-select-enable-clipboard t)

(provide 'dimitern-backups)
