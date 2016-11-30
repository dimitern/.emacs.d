;; init.el: Emacs configuration of Dimiter Naydenov.
;;

;; Added by package.el.
;(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(load "dimitern-packaging")

;; dimitern-os: OS-specific helper functions.
(load "dimitern-os")

(validate-setq
 ;; User Info
 user-full-name "Dimiter Naydenov"
 user-mail-address "dimiter@naydenov.net"
  )

;; dimitern-theme: solarized theme config.
(load "dimitern-theme")

;; dimitern-spaceline: spaceline and powerline config.
(use-package dimitern-spaceline
  :init
  (add-hook 'emacs-startup-hook #'dimitern-spaceline/emacs-theme)
  ;; This is also necessary to fix powerline in terminal mode.
  (unless (display-graphic-p)
    (add-hook 'tty-setup-hook #'dimitern-spaceline/emacs-theme))
  )

;; dimitern-startup: startup config.
(use-package dimitern-startup)

;; dimitern-backups: backups/autosaves config. 
(use-package dimitern-backups)

;; dimitern-clipboard: clipboard-related config.
(use-package dimitern-clipboard)

;; dimitern-frames: global frame-related config.
(use-package dimitern-frames
  :init
  (add-hook 'after-init-hook #'dimitern-frames/no-bars))

;; dimitern-buffers: global buffer-related config.
(use-package dimitern-buffers)

;; dimitern-files: global files-related config.
(use-package dimitern-files)

;; dimitern-navigation: navigation-related config.
(use-package dimitern-navigation)

;; dimitern-windows: global window-related config.
(use-package dimitern-windows)

;; dimitern-modes: global modes config.
(use-package dimitern-modes)

;; exec-path-from-shell: set shell environment variables.
(use-package dimitern-exec-path-from-shell)

;; dimitern-help: which-key, which-func, ivy, counsel, etc.
(use-package dimitern-help
  :defer 0.5)

;; dimitern-search-replace: isearch, anzu, wgrep, ag, visual-regexp,
;; swiper.
(use-package dimitern-search-replace
  :defer 1)
