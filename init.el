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
 ;; Increase the number of lines in the *Messages* buffer to help debugging init
 ;; issues.
 message-log-max 10000
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

;; Personal packages config.
(use-package dimitern-startup
  :load-path "lisp/"
  :init
  (use-package dimitern-backups)
  (use-package dimitern-clipboard)
  (use-package dimitern-frames)
  (use-package dimitern-modes)
  (use-package dimitern-theme)

  ;; validate: provides (validate-setq)
  (use-package validate
    :pin gnu
    :ensure t)

  ;; hydra: bindings that stick.
  (use-package hydra
    :ensure t)
  
  ;; exec-path-from-shell: set shell environment variables.
  (use-package dimitern-exec-path-from-shell)

  ;; mode-line: spacemacs-style, using spaceline+powerline.
  (use-package dimitern-mode-line)

  ;; minibuffer: uses ivy, ivy-hydra, counsel, savehist.
  (use-package dimitern-minibuffer
    :preface
    ;; Disable GC while the minibuffer is active, reset on close.
    (defun dimitern/minibuffer-setup-hook ()
      (setq gc-cons-threshold most-positive-fixnum))
    (add-hook 'minibuffer-setup-hook #'dimitern/minibuffer-setup-hook)

    ;; Enable GC on exiting the minibuffer.
    (defun dimitern/minibuffer-exit-hook ()
      (setq gc-cons-threshold dimiter/gc-cons-threshole))
    (add-hook 'minibuffer-exit-hook #'dimitern/minibuffer-exit-hook)

    )

  ;; search-replace: isearch, anzu, wgrep, ag, visual-regexp, swiper.
  (use-package dimitern-search-replace)

  )
