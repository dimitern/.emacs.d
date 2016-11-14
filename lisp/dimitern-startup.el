;; ----------------------------------------------------------------------------
;; dimitern-startup: Startup config
;; ----------------------------------------------------------------------------

;; Set user name and email.
(setq user-full-name "Dimiter Naydenov"
      user-mail-address "dimiter@naydenov.net")

;; Inhibit default startup cruft.
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message (getenv "USER"))
(setq initial-scratch-message nil)

;; Disable menu, toolbar and scroll bar by default.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Use text mode by default for new buffers.
(setq default-major-mode 'text-mode)

;; Miscellaneous
(setq auto-mode-alist (cons '("\\.mm$" . c++-mode) auto-mode-alist))
(setq c-default-style "linux")
(setq vc-follow-symlinks t)

;; Use X clipboard.
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq save-interprogram-paste-before-kill t)

;; Default indent 4 spaces per tab, use tabs.
(setq standard-indent 4)
(setq-default indent-tabs-mode t)

;; Toggle menu with F1.
(global-set-key [f1] 'menu-bar-mode)

;; Ignore ring-bell, middle-click paste.
(setq ring-bell-function '(lambda ()))
(setq mouse-yank-at-point t)

;; Double space after sentence, final newline.
(setq sentence-end-double-space nil)
(setq require-final-newline t)

;; When saving files, set execute permission if #! is in first line.
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

;; Use "y" or "n" vs "yes" or "no".
(fset 'yes-or-no-p 'y-or-n-p)

;; All additional packages setup goes below.

(use-package dimitern-customize)

(use-package dimitern-backups)

(use-package dimitern-theme)

;; Keep this last.
(message "dimitern: Startup setup done")

(provide 'dimitern-startup)
