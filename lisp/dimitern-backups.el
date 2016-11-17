;; dimitern-backups: Setup autosaves and backup files creation.
;;

(setq
 ;; Put all backups in one place.
 backup-directory-alist '(("." . "~/.emacs.d/backups"))
 ;; Save and version all backups.
 delete-old-versions -1
 version-control t
 vc-make-backup-files t
 auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
 )

(provide 'dimitern-backups)
