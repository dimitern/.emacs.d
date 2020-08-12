;; dimitern-completion.el: company and related config.
;;

; In `completion-at-point', do not pop up silly completion buffers for less
;; than five candidates.  Cycle instead.
(validate-setq completion-cycle-threshold 5)
(setq-local company-dabbrev-downcase nil)

;; ;; hippie-exp: powerful expansion and completion.
;; (use-package hippie-exp
;;   :bind (([remap dabbrev-expand] . hippie-expand))
;;   :config
;;   (validate-setq
;;    hippie-expand-try-functions-list
;;    '(try-expand-dabbrev
;;      try-expand-dabbrev-all-buffers
;;      try-expand-dabbrev-from-kill
;;      try-complete-file-name-partially
;;      try-complete-file-name
;;      try-expand-all-abbrevs
;;      try-expand-list
;;      try-complete-lisp-symbol-partially
;;      try-complete-lisp-symbol)))

;; yasnippet: snippets.
(use-package yasnippet
  :ensure t
  :defer t
  :diminish (yas-minor-mode . "Ⓨ"))

;; company: graphical (auto-)completion.
(use-package company
  :pin "melpa"
  :ensure t
  :defer t
  :config
  (validate-setq
   company-tooltip-align-annotations t
   company-tooltip-flip-when-above t
   company-show-numbers t)
  (global-company-mode))

;; company-quickhelp: show help in tooltip.
(use-package company-quickhelp
  :ensure t
  :after company
  :config (company-quickhelp-mode))

;; company-statistics: sort company candidates by statistics.
(use-package company-statistics
  :ensure t
  :pin "gnu"
  :after company
  :config (company-statistics-mode))

;; company-math: completion for Math symbols.
(use-package company-math
  :ensure t
  :after company
  :config
  ;; Add backends for math characters
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-math-symbols-latex))

;; company-rust: completion backend for Rust.
(use-package company-racer
  :ensure t
  :pin "melpa"
  :after company)

;; company-emoji: emojis completion like Git hub/Slack.
;; (use-package company-emoji
;;   :ensure t
;;   :after company
;;   :config
;;   (add-to-list 'company-backends 'company-emoji)

;;   (defun dimitern-company-emoji-no-unicode ()
;;     "Turn off unicode emoji for this buffer."
;;     (setq-local company-emoji-insert-unicode nil))

;;   (add-hook 'gfm-mode-hook #'dimitern-company-emoji-no-unicode))

(provide 'dimitern-completion)
