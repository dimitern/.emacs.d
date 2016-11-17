;; dimitern-frames.el: Global window system and frames config.
;;

;; No tool bar, menu bar, and scroll bars. On OS X we preserve the
;; menu bar, since the top menu bar is always visible anyway, and we'd
;; just empty it which is rather pointless.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Allow toggling the menu bar with F1.
(global-set-key [f1] 'menu-bar-mode)

(setq
 ;; Use X clipboard.
 x-select-enable-clipboard t
 x-select-enable-primary t
 save-interprogram-paste-before-kill t
 ;; Ignore ring-bell, middle-click paste.
 ring-bell-function '(lambda ())
 mouse-yank-at-point t
 )

(provide 'dimitern-frames)
