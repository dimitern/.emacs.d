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

  (spaceline-define-segment python-virtualenvwrapper
    "The current python venv.  Works with `virtualenvwrapper'."
    (when (and active
               (eq 'python-mode major-mode)
               (bound-and-true-p venv-current-name))
      (let ((name venv-current-name))
        (propertize name
                    'face 'spaceline-python-venv
                    'help-echo "Virtual environment (via virtualenvwrapper)"))))

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
     ((python-virtualenvwrapper :fallback python-venv :when active) :separator "ve: ")
     ((which-function projectile-root) :separator " @ ")
     ((minor-modes :separator spaceline-minor-modes-separator) :when active))
   ;; Right segment (the unimportant stuff)
   '((version-control :when active)))

  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-dimitern))))

   (validate-setq
   powerline-height (truncate (* 1.0 (frame-char-height)))
   powerline-default-separator 'utf-8)

   (require 'powerline))

(provide 'dimitern-spaceline)
