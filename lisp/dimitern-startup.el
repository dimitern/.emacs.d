;; dimitern-startup.el: Startup Emacs configuration.
;;

(when (version< emacs-version "25")
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version))

;; Use "y" or "n" vs "yes" or "no".
(fset 'yes-or-no-p 'y-or-n-p)
;; Opt out from the startup message in the echo area by simply
;; disabling this ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)

;; Load my customized file.
(setq custom-file "~/.emacs.d/emacs-customize.el")
(unless (file-exists-p "~/.emacs.d/emacs-customize.elc")
  (byte-compile-file custom-file))
(load-file custom-file)

(provide 'dimitern-startup)
