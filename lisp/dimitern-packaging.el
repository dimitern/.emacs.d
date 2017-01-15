;; dimitern-packaging.el: Packaging related config.
;;

;; Save the original GC threshold to restore it later.
(defvar dimitern/gc-cons-threshold gc-cons-threshold
  "Original gc-cons-threshold value.")
;; Increse GC threshold to speed up init loading.
(setq gc-cons-threshold most-positive-fixnum)

;; Debugging & load-time diagnostics.
(add-hook 'after-init-hook
          (lambda ()
            (message "Time to load init file: %s"
                     (emacs-init-time))
            (setq
             gc-cons-threshold dimitern/gc-cons-threshold)))

(defun dimitern-help/minibuffer-setup-hook ()
  "Disable GC while the minibuffer is active, reset on close."
  (setq gc-cons-threshold most-positive-fixnum))
(add-hook 'minibuffer-setup-hook #'dimitern-help/minibuffer-setup-hook)

(defun dimitern-help/minibuffer-exit-hook ()
  "Enable GC on exiting the minibuffer."
  (setq gc-cons-threshold dimitern/gc-cons-threshold))
(add-hook 'minibuffer-exit-hook #'dimitern-help/minibuffer-exit-hook)

;; This sets load-path, needed before setting the package-archives
;; below.
(package-initialize)

(setq
 ;; Don't load outdated byte code.
 load-prefer-newer t
 ; Don't activate installed packages on startup.
 package-enable-at-startup nil
 ;; Setup package archives to include MELPA, then ELPA.
 package-archives (append
                   package-archives
                   '(("melpa"        . "https://melpa.org/packages/")
                     ("melpa-stable" . "https://stable.melpa.org/packages/")
                     ("org"          . "http://orgmode.org/elpa/")
                     ("gnu"          . "http://elpa.gnu.org/packages/")))
 ;; use-package: log what gets loaded and pin to MELPA by default.
 use-package-verbose t
 use-package-always-pin "melpa"  ;; prefer MELPA latest.
 use-package-minimum-reported-time 0.1
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

;; Enable use-package, bind-key, and diminish.
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; auto-compile: automatic byte compilation.
(use-package auto-compile
  :ensure t
  :init
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; validate: provides (validate-setq)
(use-package validate
  :pin gnu
  :ensure t)

;; paradox: better package manager.
(use-package paradox
  :ensure t
  :defer t
  :bind (("C-c M-a p" . paradox-list-packages)
         ("C-c M-a P" . paradox-upgrade-packages))
  :config
  (validate-setq
   paradox-execute-asynchronously nil   ; No async update, please
   paradox-spinner-type 'moon           ; Fancy spinner
   ;; Show all possible counts
   paradox-display-download-count t
   paradox-display-star-count t
   ;; Don't star automatically
   paradox-automatically-star nil
   ;; Hide download button, and wiki packages
   paradox-use-homepage-buttons nil     ; Can type v instead
   paradox-hide-wiki-packages t))
