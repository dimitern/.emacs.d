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

;; hydra: bindings that stick.
(use-package hydra
  :ensure t)

;; spaceline: cool spacemacs-style mode-line
(use-package spaceline-config
  :ensure spaceline)

;; powerline: the power-horse of spaceline.
(use-package powerline
  :ensure t
  :after spaceline-config
  :init
  (spaceline-emacs-theme)
  :config
  (validate-setq
   powerline-height (truncate (* 1.0 (frame-char-height)))
   powerline-default-separator 'utf-8))

;; dimitern-startup: startup config.
(use-package dimitern-startup
  :load-path "lisp/")

;; dimitern-backups: backups/autosaves config. 
(use-package dimitern-backups)

;; dimitern-clipboard: clipboard-related config.
(use-package dimitern-clipboard)

;; dimitern-frames: global frame-related config.
(use-package dimitern-frames
  :after tool-bar)

;; dimitern-modes: global modes config.
(use-package dimitern-modes)

;; dimitern-theme: theme setup (solarized dark).
(use-package dimitern-theme)

;; exec-path-from-shell: set shell environment variables.
(use-package dimitern-exec-path-from-shell)

;; dimitern-mode-line: additional mode line config.
(use-package dimitern-mode-line
  :after powerline)

;; dimitern-minibuffer: minibuffer config with ivy, ivy-hydra,
;; counsel, savehist.
(use-package dimitern-minibuffer)

;; dimitern-search-replace: isearch, anzu, wgrep, ag, visual-regexp,
;; swiper.
(use-package dimitern-search-replace)

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

;; page-break-lines: turn page breaks (^L) into lines.
(use-package page-break-lines
  :ensure t
  :init
  (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

;; beacon: highlight cursor position in buffer (when switching).
(use-package beacon
  :ensure t
  :init
  (beacon-mode 1)
  :diminish beacon-mode)

;; hl-line: highlight the current line in buffer.
(use-package hl-line
  :init
  (global-hl-line-mode 1))

;; stripe-buffer: add stripes to buffers (lists).
(use-package stripe-buffer
  :ensure t
  :init
  (add-hook 'dired-mode-hook #'stripe-buffer-mode))

;; face-remap: face remapping (text scaling)
(use-package face-remap
  :bind (("C-c w z" . text-scale-adjust)))
