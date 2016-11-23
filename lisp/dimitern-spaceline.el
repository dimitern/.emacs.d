;; dimitern-spaceline.el: spaceline and powerline config.
;;

;; spaceline: cool spacemacs-style mode-line
(use-package spaceline-config
  :ensure spaceline)

;;;###autoload
(defun dimitern-spaceline/emacs-theme ()
  "Enable Emacs spaceline theme."
  (require 'spaceline-config)
  (spaceline-emacs-theme)

  ;; powerline: the power-horse of spaceline.
  (use-package powerline
    :ensure t
    :init
    (validate-setq
     powerline-height (truncate (* 1.0 (frame-char-height)))
     powerline-default-separator 'utf-8))
  )

(provide 'dimitern-spaceline)
