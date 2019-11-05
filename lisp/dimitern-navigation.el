;; dimitern-navigation.el: navigation-related config.
;;

(validate-setq
 scroll-conservatively 1000          ; Never recenter the screen while scrolling
 scroll-error-top-bottom t           ; Move to beg/end of buffer before
                                        ; signalling an error
 ;; These settings make trackpad scrolling on OS X much more predictable
 ;; and smooth
 mouse-wheel-progressive-speed nil
 mouse-wheel-scroll-amount '(1))

;; Turn on horizontal scrolling with mouse wheel
(global-set-key (kbd "<mouse-6>") 'scroll-right)
(global-set-key (kbd "<mouse-7>") 'scroll-left)

;; ivy-pages: jump to pages with ivy.
(use-package ivy-pages
  :ensure t
  :defer t
  :bind (("C-c j p" . ivy-pages)))

;; avy-jump: jump to characters in buffers.
(use-package avy-jump
  :ensure avy
  :defer t
  :bind (("C-c j w" . avy-goto-word-1)
         ("C-c j l" . avy-goto-line)
         ("C-c j b" . avy-pop-mark)
         ("C-c j j" . avy-goto-char-2)))

;; ace-line: fast link jumping.
(use-package ace-link
  :ensure t
  :defer t
  :bind (:map Info-mode-map ("C-c m l" . ace-link-info)
         :map help-mode-map ("C-c m l" . ace-link-help)))

;; outline: navigate outlines in buffers
(use-package outline
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'outline-minor-mode))
  :diminish outline-minor-mode)

;; nlinum: line numbers in display margin.
(use-package nlinum
  :ensure t
  :pin "gnu"
  :init
  (global-nlinum-mode)
  :bind (("C-c t l" . nlinum-mode))
  :diminish (nlinum-mode . "ⓝ"))

(use-package treemacs
  :ensure t
  :pin "melpa"
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (treemacs--find-python3) 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (treemacs--find-python3))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :pin "melpa"
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :pin "melpa"
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :pin "melpa"
  :ensure t)

(provide 'dimitern-navigation)
