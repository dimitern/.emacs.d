;; init.el: Emacs configuration of Dimiter Naydenov.
;;

;; Save the original GC threshold to restore it later.
(defvar dimitern/gc-cons-threshold gc-cons-threshold
  "Original gc-cons-threshold value.")
;; Increse GC threshold to speed up init loading.
(setq gc-cons-threshold most-positive-fixnum)

;; Debugging & load-time diagnostics.
(add-hook 'after-init-hook (lambda ()
			     (message "Time to load init file: %s"
				      (emacs-init-time))
			     (setq gc-cons-threshold dimitern/gc-cons-threshold)))

;; Initialize package, which also sets load-path, needed before setting the
;; package-archives below.
(require 'package)
(package-initialize)

(setq
 ;; User Info
 user-full-name "Dimiter Naydenov"
 user-mail-address "dimiter@naydenov.net"
 ;; Packaging
 load-prefer-newer t              ; don't load outdated byte code.
 package-enable-at-startup nil    ; don't activate installed packages.
 package-archives (append package-archives
			  '(("melpa" . "http://melpa.org/packages/")
			    ("gnu" . "http://elpa.gnu.org/packages/")
			    ("elpy" . "http://jorgenschaefer.github.io/packages/")))
 ;; use-package - log what gets loaded.
 use-package-verbose t
 use-package-always-pin "melpa"  ;; prefer MELPA latest.
 use-package-minimum-reported-time 0.05
 ;; Increase the number of lines in the *Messages* buffer to help debugging init
 ;; issues.
 message-log-max 10000
 ;; Do not load site file.
 site-run-file nil
 )

;; Install use-package if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; validate: provides (validate-setq)
(use-package validate
  :pin gnu
  :ensure t)

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
