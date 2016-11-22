;; dimitern-backups: Setup autosaves and backup files creation.
;;

(validate-setq
 ;; Put all backups, autosaves in one place.
 backup-directory-alist '(("." . "~/.emacs.d/backups"))
 auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
 ;; Make backups, including for VC-managed files.
 make-backup-files t
 vc-make-backup-files t
 ;; Version all create backups (with incremental index as suffix).
 version-control t
 ;; Delete old backup versions automatically.
 delete-old-versions t
 ;; Keep the oldest and newest 5 backups only.
 kept-old-versions 5
 kept-new-versions 5
 ;; Always copy, never rename files when backing up.
 backup-by-copying t
 backup-by-copying-when-linked t
 backup-by-copying-when-mismatch t
 )

(provide 'dimitern-backups)
