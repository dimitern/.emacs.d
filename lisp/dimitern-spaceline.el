;; dimitern-spaceline.el: spaceline and powerline config.
;;

;; spaceline: cool spacemacs-style mode-line
(use-package spaceline
  :defer t
  :ensure t)

;; spaceline-config: themes for spaceline.
(use-package spaceline-config
  :defer t)

;; powerline: the power-horse of spaceline.
(use-package powerline
  :defer t)

(defun dimitern-spaceline/emacs-theme ()
  "Enable Emacs spaceline theme with deferred loading to speed up
initial startup time."
  (interactive)
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  (validate-setq
   powerline-height (truncate (* 1.0 (frame-char-height)))
   powerline-default-separator 'utf-8)
  (require 'powerline))

(provide 'dimitern-spaceline)
