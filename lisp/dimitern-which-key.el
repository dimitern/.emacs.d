;; dimitern-which-key.el package config.
;;

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  ;; Use a popup at the frame bottom.
  (which-key-setup-side-window-bottom)
  (validate-setq
   which-key-idle-delay 0.4
   which-key-sort-order 'which-key-prefix-then-key-order
   ;; Let's go unicode :)
   which-key-key-replacement-alist
   '(("<\\([[:alnum:]-]+\\)>" . "\\1")
     ("up"                    . "↑")
     ("right"                 . "→")
     ("down"                  . "↓")
     ("left"                  . "←")
     ("DEL"                   . "⌫")
     ("deletechar"            . "⌦")
     ("RET"                   . "⏎"))
   which-key-description-replacement-alist
   '(("Prefix Command" . "prefix")
     ;; Lambdas
     ("\\`\\?\\?\\'"   . "λ")
     ;; Prettify hydra entry points
     ("/body\\'"       . "|=")
     ;; Drop/shorten package prefixes
     ("magit-"         . "ma-")))

  (which-key-declare-prefixes
    ;; Prefixes for global prefixes and minor modes
    "C-c @" "outline"
    "C-c !" "flycheck"
    "C-c 8" "typo"
    "C-c 8 -" "typo/dashes"
    "C-c 8 <" "typo/left-brackets"
    "C-c 8 >" "typo/right-brackets")

  ;; Prefixes for major modes
  (which-key-declare-prefixes-for-mode 'markdown-mode
    "C-c TAB" "markdown/images"
    "C-c C-a" "markdown/links"
    "C-c C-c" "markdown/process"
    "C-c C-s" "markdown/style"
    "C-c C-t" "markdown/header"
    "C-c C-x" "markdown/structure"
    "C-c m" "markdown/personal")

  (which-key-declare-prefixes-for-mode 'emacs-lisp-mode
    "C-c m" "elisp/personal"
    "C-c m e" "eval")

  (which-key-declare-prefixes-for-mode 'js2-mode
    "C-c m" "js/personal"
    "C-c m r" "refactor")

  (which-key-declare-prefixes-for-mode 'web-mode
    "C-c C-a" "web/attributes"
    "C-c C-b" "web/blocks"
    "C-c C-d" "web/dom"
    "C-c C-e" "web/element"
    "C-c C-t" "web/tags")

  :diminish which-key-mode)

(provide 'dimitern-which-key)
