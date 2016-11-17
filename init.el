;; init.el: Emacs configuration of Dimiter Naydenov.
;;

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
 ;; Increase the numer of lines in the *Messages* buffer to help debugging init
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

;; Debugging & load-time diagnostics.
(add-hook 'after-init-hook (lambda () (message "Time to load init file: %s"
					       (emacs-init-time))))
;; Personal packages config.
(use-package dimitern-startup
  :load-path "lisp/"
  :config
  (use-package dimitern-backups)
  (use-package dimitern-history)
  (use-package dimitern-frames)
  (use-package dimitern-modes)
  (use-package dimitern-theme)

  ;; validate - provides (validate-setq)
  (use-package validate
    :ensure t)

  ;; exec-path-from-shell - set shell environment variables.
  (use-package dimitern-exec-path-from-shell)

  ;; which-key: show help popups for prefix keys.
  (use-package dimitern-which-key)
  
  ;; which-func: show the current elisp function in mode line.
  (use-package dimitern-which-func)

  ;; spaceline: spacemacs mode line (uses powerline).
  (use-package dimitern-spaceline)

  )
