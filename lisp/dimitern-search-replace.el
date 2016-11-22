;; dimitern-search-replace.el: config related to searching/replacing.
;;

;; anzu: position/matches count for isearch.
(use-package anzu
  :ensure t
  :defer 1
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :config
  (global-anzu-mode)
  (setq anzu-cons-mode-line-p nil)
  :diminish anzu-mode)

;; isearch: search buffers.
(use-package "isearch"
  ;; Defer because `isearch' is not a feature and we don't want to
  ;; `require' it
  :defer t
  :init
  ;; `:diminish' doesn't work for isearch, because it uses
  ;; eval-after-load on the feature name, but isearch.el does not
  ;; provide any feature. For the same reason we have to use `:init',
  ;; but isearch is always loaded anyways.
  (diminish 'isearch-mode)

  (validate-setq
   ;; Please, isearch, let me scroll during search
   isearch-allow-scroll t
   ;; Fold unicode characters to ASCII while searching
   search-default-mode #'char-fold-to-regexp))

;; swiper: isearch with overview.
(use-package swiper
  :ensure t
  :defer t
  :bind (([remap isearch-forward] . swiper)))

;; visual-regexp: regexp replace with in-buffer display.
(use-package visual-regexp
  :ensure t
  :bind (("C-c s r" . vr/query-replace)
         ("C-c s R" . vr/replace)))

;; ag: the silver searcher (grep on steroids)
(use-package ag
  :ensure t
  :config
  (validate-setq
   ag-highlight-search t
   ag-reuse-buffers t)
  (add-hook 'ag-mode-hook 'wgrep-ag-setup))

;; wgrep-ag: writtable ag buffer
(use-package wgrep-ag
  :config
  (validate-setq
   wgrep-auto-save-buffer t)
  :ensure t)

;; wgrep: writtable grep buffer
(use-package wgrep
  :ensure t)

(provide 'dimitern-search-replace)
