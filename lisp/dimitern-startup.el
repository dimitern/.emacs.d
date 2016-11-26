;; dimitern-startup.el: Startup Emacs configuration.
;;

(when (version< emacs-version "25")
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version))

;; Use "y" or "n" vs "yes" or "no".
(fset 'yes-or-no-p 'y-or-n-p)
;; Opt out from the startup message in the echo area by simply
;; disabling this ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)

(validate-setq
 ;; Inhibit usual startup cruft.
 inhibit-default-init t
 inhibit-startup-echo-area-message (getenv "USER")
 inhibit-startup-screen t
 initial-scratch-message nil
 )

(use-package dimitern-custom)

(provide 'dimitern-startup)
