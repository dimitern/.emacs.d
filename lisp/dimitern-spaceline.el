;; dimitern-spaceline.el (and powerline) package config.
;;

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-compile
   'dimitern
   ;; Left side of the mode line (all the important stuff)
   '(((buffer-modified buffer-size input-method) :face highlight-face)
     anzu
     '(buffer-id remote-host buffer-encoding-abbrev)
     ((point-position line-column buffer-position selection-info)
      :separator " | ")
     major-mode
     process
     (flycheck-error flycheck-warning flycheck-info)
     (python-pyvenv :fallback python-pyenv)
     ((which-function projectile-root) :separator " @ ")
     ((minor-modes :separator spaceline-minor-modes-separator) :when active))
   ;; Right segment (the unimportant stuff)
   '((version-control :when active)))

  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-dimitern)))))

(use-package powerline
  :ensure t
  :after spaceline-config
  :config (validate-setq
           powerline-height (truncate (* 1.0 (frame-char-height)))
           powerline-default-separator 'utf-8))

(provide 'dimitern-spaceline)
