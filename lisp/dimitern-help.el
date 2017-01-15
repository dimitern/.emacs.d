;; dimitern-help.el: help for bindings and other config.
;;

;; hydra: bindings that stick.
(use-package hydra
  :ensure t)

;; which-key: show help popups for prefix keys.
(use-package which-key
  :ensure t
  :init
  ;; Use a popup at the frame bottom.
  (which-key-setup-side-window-bottom)
  (which-key-mode)
  :config
  ;; Show current line/column number in mode-line.
  (line-number-mode)
  (column-number-mode)
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
     ("dimitern-"      . "")
     ("magit-"         . "ma-")))

  (which-key-declare-prefixes
    ;; Prefixes for global prefixes and minor modes
    "C-x C-a" "gud/pdb"
    "C-c @" "outline"
    "C-c !" "flycheck"
    "C-c 8" "typo"
    "C-c 8 -" "typo/dashes"
    "C-c 8 <" "typo/left-brackets"
    "C-c 8 >" "typo/right-brackets"
    ;; Prefixes for my personal bindings
    "C-c M-a applications"
    "C-c a" "org-agenda"
    "C-c b" "org-iswitchb"
    "C-c M-b" "buffers"
    "C-c c" "org-capture"
    "C-c M-c" "compile-and-comments"
    "C-c e" "errors"
    "C-c f" "files"
    "C-c f v" "variables"
    "C-c C-g" "magit-status"
    "C-c g" "git"
    "C-c g g" "github/gist"
    "C-c h" "help"
    "C-c i" "insert"
    "C-c i l" "licenses"
    "C-c j" "jump"
    "C-c l" "org-store-link"
    "C-c M-l" "language/spelling"
    "C-c m" "major mode"
    "C-c C-v" "org-babel"
    "C-c o" "org-mode"
    "C-c M-/" "cursors"
    "C-c M-/ i" "cursors/insert"
    "C-c p" "projects"
    "C-c p s" "projects/search"
    "C-c p x" "projects/execute"
    "C-c p 4" "projects/other-window"
    "C-c s" "search"
    "C-c t" "toggle"
    "C-c w" "windows/frames"
    "C-c x" "text")

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

  (which-key-declare-prefixes-for-mode 'org-mode
    "C-c a"   "org-agenda"
    "C-c b"   "org-iswitchb"
    "C-c c"   "org-capture"
    "C-c C-e" "org-export"
    "C-c l"   "org-store-link"
    "C-c C-a" "org-attach")

  :diminish which-key-mode)

;; which-func: show the current elisp function in mode line.
(use-package which-func
  :ensure t
  :init
  (which-function-mode)
  :config
  (validate-setq
   which-func-unknown "⊥"               ; The default is really boring…
   which-func-format
   `((:propertize (" ➤ " which-func-current)
                  local-map ,which-func-keymap
                  face which-func
                  mouse-face mode-line-highlight
                  help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end"))))

;; ivy: minibuffer completion.
(use-package ivy
  :ensure t
  :bind (("C-c M-b r" . ivy-resume))
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
         ([remap find-file]                . counsel-find-file)
         ([remap describe-function]        . counsel-describe-function)
         ([remap describe-variable]        . counsel-describe-variable)
         ([remap info-lookup-symbol]       . counsel-info-lookup-symbol)
         ([remap completion-at-point]      . counsel-company)
         ("C-c f L"                        . counsel-load-library)
         ("C-c f r"                        . counsel-recentf)
         ("C-c i 8"                        . counsel-unicode-char)
         ("C-c s a"                        . counsel-ag)
         ("C-c g L"                        . counsel-git-log)
         ("C-c j t"                        . counsel-imenu)))

(provide 'dimitern-help)
