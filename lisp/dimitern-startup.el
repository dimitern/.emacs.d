;; dimitern-startup.el: Startup Emacs configuration.
;;

(when (version< emacs-version "25")
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version))

(setq
 ;; Inhibit default startup cruft.
 inhibit-splash-screen t
 inhibit-startup-message t
 inhibit-startup-echo-area-message (getenv "USER")
 initial-scratch-message nil
 ;; Disable site default settings.
 inhibit-default-init t
 )

;; Use "y" or "n" vs "yes" or "no".
(fset 'yes-or-no-p 'y-or-n-p)
;; Opt out from the startup message in the echo area by simply
;; disabling this ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)

;; Load and/or create empty custom file.
(let ((fn (expand-file-name "~/.emacs.d/emacs-customize.el")))
  (when (not (file-exists-p fn))
    (shell-command (concat "touch " fn)))
  (setq custom-file fn))
(load custom-file 'noerror)

(provide 'dimitern-startup)
