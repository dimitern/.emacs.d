;; dimitern-frames.el: Global window system and frames config.
;;

(validate-setq
 ;; No audible bell (ding), use visual bell instead.
 visible-bell t
 )

;; No tool bar by default.
(use-package tool-bar
  :init
  (tool-bar-mode -1))

;; No scroll bar by default.
(use-package scroll-bar
  :init
  (scroll-bar-mode -1))

;; No menu bar unless on darwin GUI.
(use-package menu-bar
  :unless (and
	   (eq system-type 'darwin)
	   (display-graphic-p))
  :init
  (menu-bar-mode -1)
  ;; Allow toggling the menu bar with F1.
  (global-set-key [f1] 'menu-bar-mode))

(provide 'dimitern-frames)
