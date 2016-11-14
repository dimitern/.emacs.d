;; ----------------------------------------------------------------------------
;; dimitern-backups: Setup autosaves and backup files creation
;; ----------------------------------------------------------------------------

;; Store all backup and autosave files in the /tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(provide 'dimitern-backups)
