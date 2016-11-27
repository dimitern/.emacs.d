;; dimitern-backups: Setup autosaves, history, and backup files
;; creation.
;;

(defconst dimitern/backups-dir (expand-file-name "~/.emacs.d/backups/")
  "Default directory for backup files.")

(defconst dimitern/autosaves-dir (expand-file-name "~/.emacs.d/autosaves/")
  "Default directory for auto-saved files.")

(defconst dimitern/history-file (expand-file-name "~/.emacs.d/savehist")
  "File to use for storing history, recentf, kill ring, etc.")

(defun dimitern/ensure-path (path)
  "Ensure the directory specified with the given absolute `path'
exists, creating it and its parent(s), when it does not exist."
  (unless (file-exists-p path)
    (make-directory path 'with-parents)))

;; savehist - minibuffer history.
(use-package savehist
  :ensure t
  :init
  ;; Ensure autosaves and backups directories exist. 
  (mapc 'dimitern/ensure-path
	`(,dimitern/backups-dir
	  ,dimitern/autosaves-dir))
  (validate-setq
   ;; Put all backups, autosaves in one place.
   backup-directory-alist `(("." . ,dimitern/backups-dir))
   auto-save-file-name-transforms `((".*" ,dimitern/autosaves-dir t))
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
  (savehist-mode 1)
  :config
  (validate-setq
   savehist-file dimitern/history-file
   use-dialog-box nil
   history-length t			; unlimited length.
   history-delete-duplicates t		; no duplicates saved.
   savehist-save-minibuffer-history t
   savehist-additional-variables
   '(kill-ring
     search-ring
     regexp-search-ring)))

(provide 'dimitern-backups)
