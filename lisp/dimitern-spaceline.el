;; dimitern-spaceline.el: spaceline and powerline config.
;;

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

(provide 'dimitern-spaceline)
