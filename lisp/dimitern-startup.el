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

(message "Startup setup done")

(provide 'dimitern-startup)
