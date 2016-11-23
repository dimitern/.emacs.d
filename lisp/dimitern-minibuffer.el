;; dimitern-minibuffer.el: minibuffer completions and history config.
;;

;; ivy: minibuffer completion.
(use-package ivy
  :ensure t
  :bind (("C-c b r" . ivy-resume))
  :init
  (ivy-mode 1)
  ;; Include recentf and bookmarks to switch buffer, and tune the
  ;; count format.
  (validate-setq
   ivy-use-virtual-buffers t
   ivy-count-format "(%d/%d) "
   )
  :diminish ivy-mode)

;; ivy-hydra: hydra bindings for ivy buffer.
(use-package ivy-hydra
  :ensure t
  :after ivy
  :bind (:map ivy-minibuffer-map
         ("C-o" . hydra-ivy/body)))

;; counsel: ivy-powered commands completion.
(use-package counsel
  :ensure t
  :bind (([remap execute-extended-command] . counsel-M-x)
         ([remap find-file] . counsel-find-file)
         ([remap describe-function] . counsel-describe-function)
         ([remap describe-variable] . counsel-describe-variable)
         ([remap info-lookup-symbol] . counsel-info-lookup-symbol)
         ("C-c f L" . counsel-load-library)
         ("C-c i 8" . counsel-unicode-char)
         ("C-c s a" . counsel-ag)
         ("C-c j t" . counsel-imenu)))

;; savehist - minibuffer history.
(use-package savehist
  :ensure t
  :init (savehist-mode 1)
  :config
  (validate-setq
   savehist-file "~/.emacs.d/savehist"
   use-dialog-box nil
   history-length t
   history-delete-duplicates t
   savehist-save-minibuffer-history t
   savehist-additional-variables
   '(kill-ring
     search-ring
     regexp-search-ring)))

(defun dimitern-minibuffer/setup-hook ()
  "Disable GC while the minibuffer is active, reset on close."
  (setq gc-cons-threshold most-positive-fixnum))
(add-hook 'minibuffer-setup-hook #'dimitern-minibuffer/setup-hook)

(defun dimitern-minibuffer/exit-hook ()
  "Enable GC on exiting the minibuffer."
  (setq gc-cons-threshold dimitern/gc-cons-threshold))
(add-hook 'minibuffer-exit-hook #'dimitern-minibuffer/exit-hook)

(provide 'dimitern-minibuffer)
