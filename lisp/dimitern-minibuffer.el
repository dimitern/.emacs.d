;; dimitern-minibuffer.el: minibuffer completions and history config.
;;

;; ivy: minibuffer completion.
(use-package ivy
  :ensure t
  :init (ivy-mode 1)
  :bind (("C-c b r" . ivy-resume))
  :config
  ;; Include recentf and bookmarks to switch buffer, and tune the
  ;; count format.
  (validate-setq ivy-use-virtual-buffers t
                 ivy-count-format "(%d/%d) ")
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

;; Disable GC while the minibuffer is active, reset on close.
(defun dimitern/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun dimitern/minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'dimitern/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'dimitern/minibuffer-exit-hook)

(provide 'dimitern-minibuffer)
