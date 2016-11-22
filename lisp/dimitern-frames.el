;; dimitern-frames.el: Global window system and frames config.
;;

(validate-setq
 ;; No audible bell (ding), use visual bell instead.
 visible-bell t
 )

;; No tool bar by default.
(use-package tool-bar
  :config
  (tool-bar-mode -1))

;; No scroll bar by default.
(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

;; No menu bar unless on darwin.
(use-package menu-bar
  :unless (eq system-type 'darwin)
  :config
  (menu-bar-mode -1)
  ;; Allow toggling the menu bar with F1.
  (global-set-key [f1] 'menu-bar-mode))

(provide 'dimitern-frames)
