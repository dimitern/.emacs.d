;; init.el: Emacs configuration of Dimiter Naydenov.
;;

;; Added by package.el.
;(package-initialize)

(load "~/.emacs.d/lisp/dimitern-packaging.el" 'no-error)

(validate-setq
 ;; User Info
 user-full-name "Dimiter Naydenov"
 user-mail-address "dimiter@naydenov.net"
 )

;; dimitern-os: OS-specific helper functions.
(use-package dimitern-os
  :load-path "lisp/")

;; dimitern-spaceline: spaceline and powerline config.
(use-package dimitern-spaceline
  :init
  (add-hook 'emacs-startup-hook #'dimitern-spaceline/emacs-theme)
  ;; This is also necessary to fix powerline in terminal mode.
  (unless (display-graphic-p)
    (add-hook 'tty-setup-hook #'dimitern-spaceline/emacs-theme))
  :load-path "lisp/")

;; dimitern-startup: startup config.
(use-package dimitern-startup
  :load-path "lisp/")

;; dimitern-backups: backups/autosaves config. 
(use-package dimitern-backups
  :load-path "lisp/")

;; dimitern-clipboard: clipboard-related config.
(use-package dimitern-clipboard
  :load-path "lisp/")

;; dimitern-frames: global frame-related config.
(use-package dimitern-frames
  :init
  (add-hook 'after-init-hook #'dimitern-frames/no-bars)
  :load-path "lisp/")

;; dimitern-buffers: global buffer-related config.
(use-package dimitern-buffers
  :load-path "lisp/")

;; dimitern-windows: global window-related config.
(use-package dimitern-windows
  :load-path "lisp/")

;; dimitern-modes: global modes config.
(use-package dimitern-modes
  :load-path "lisp/")

;; dimitern-theme: theme setup (solarized dark).
(use-package dimitern-theme
  :load-path "lisp/")

;; exec-path-from-shell: set shell environment variables.
(use-package dimitern-exec-path-from-shell
  :load-path "lisp/")

;; dimitern-help: which-key, which-func, ivy, counsel, etc.
(use-package dimitern-help
  :defer 1
  :load-path "lisp/")

;; dimitern-search-replace: isearch, anzu, wgrep, ag, visual-regexp,
;; swiper.
(use-package dimitern-search-replace
  :load-path "lisp/")
