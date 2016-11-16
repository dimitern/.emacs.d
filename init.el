;; init.el: Emacs configuration of Dimiter Naydenov.
;;

;; User Info
(setq user-full-name "Dimiter Naydenov")
(setq user-mail-address "dimiter@naydenov.net")

;; Please don't load outdated byte code
(setq load-prefer-newer t)

;; Install use-package if necessary
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives (append package-archives
			 '(("melpa" . "http://melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("elpy" . "http://jorgenschaefer.github.io/packages/"))))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(setq use-package-verbose t)
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; Debugging & load-time diagnostics.
(add-hook 'after-init-hook (lambda () (message "Time to load init file: %s"
                                               (emacs-init-time))))
(setq message-log-max 10000)

;; Personal packages config.
(use-package dimitern-startup
  :load-path "lisp/")

(use-package dimitern-backups)
(use-package dimitern-history)
(use-package dimitern-frames)
(use-package dimitern-modes)
(use-package dimitern-theme)
