;; dimitern-navigation.el: navigation-related config.
;;

(validate-setq
 scroll-conservatively 1000          ; Never recenter the screen while scrolling
 scroll-error-top-bottom t           ; Move to beg/end of buffer before
                                        ; signalling an error
 ;; These settings make trackpad scrolling on OS X much more predictable
 ;; and smooth
 ;; mouse-wheel-progressive-speed nil
 ;; mouse-wheel-scroll-amount '(1)
 )

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


(defun dimitern/treemacs--ignore-pycache-dirs (FILENAME FULL-PATH)
  "Return t when 'FILENAME` or `FULL-PATH' contain `__pycache__'."
  (if (or
       (string-match-p "__pycache__" FILENAME)
       (string-match-p "__pycache__" FULL-PATH))
      t))


(use-package treemacs
  :ensure t
  :pin "melpa"
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config

  (defvar treemacs-autoadjust-width-mode t)

  ;; See: https://github.com/Alexander-Miller/treemacs/issues/552#issuecomment-546088714
  (define-minor-mode treemacs-autoadjust-width-mode
    "Autoadjust width of the treemacs window in order to match
   the max width of its content"
    (if treemacs-autoadjust-width-mode
        (treemacs-adjust-width-start)
      (treemacs-adjust-width-stop)))

  (defun treemacs-buffer-max-width ()
    (let ((max -1) (count 0))
      (with-current-buffer (treemacs-get-local-buffer)
        (save-excursion
          (goto-char 0)
          (while (= 0 (forward-line 1))
            (setf max (max max (- (point-at-eol) (point-at-bol))))
            (cl-incf count))
          (treemacs-log "Max: %s Lines: %s" max count)))
      max))

  (defun treemacs-autoadjust-width-setter ()
    "Set the treemacs width variable and update the window value."
    (let ((width (treemacs-buffer-max-width)))
      (setq treemacs-width width)
      (treemacs--set-width width)))

  (defun treemacs-adjust-width-start ()
    "Start a timer triggering adjust width function."
    (setq treemacs-autoadjust-width-timer
          (run-with-timer 3 3 (lambda () (treemacs-autoadjust-width-setter)))))

  (defun treemacs-adjust-width-stop ()
    "Cancel the timer and set timer to nil."
    (when treemacs-autoadjust-width-timer
      (cancel-timer treemacs-autoadjust-width-timer)
      (setq treemacs-autoadjust-width-timer nil)))

  (progn

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (add-to-list 'treemacs-ignored-file-predicates 'dimitern/treemacs--ignore-pycache-dirs)

    (treemacs-follow-mode nil)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (treemacs-autoadjust-width-mode t)
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
