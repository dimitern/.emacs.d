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
  (nlinum-mode 1)
  :bind (("C-c t l" . nlinum-mode)))

(provide 'dimitern-navigation)
