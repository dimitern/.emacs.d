;; dimitern-backups: Setup autosaves and backup files creation.
;;

;; Put all backups in one place.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Save and version all backups.
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(provide 'dimitern-backups)
