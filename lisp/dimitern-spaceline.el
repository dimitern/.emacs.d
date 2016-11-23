;; dimitern-spaceline.el: spaceline and powerline config.
;;

;;;###autoload
(defun dimitern-spaceline/emacs-theme ()
  "Enable Emacs spaceline theme. Defined as autoload to speed up
initial startup time."
  (interactive)

  ;; spaceline: cool spacemacs-style mode-line
  (use-package spaceline-config
    :ensure spaceline
    :config
    (spaceline-emacs-theme))

  ;; powerline: the power-horse of spaceline.
  (use-package powerline
    :ensure t
    :after spaceline-config
    :config
    (validate-setq
     powerline-height (truncate (* 1.0 (frame-char-height)))
     powerline-default-separator 'utf-8)))

(provide 'dimitern-spaceline)
