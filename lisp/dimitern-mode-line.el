;; dimitern-mode-line.el: mode-line config
;;

;; spaceline: cool spacemacs-style mode-line.
(use-package spaceline-config
  :ensure spaceline
  :config
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
     (python-pyvenv :fallback python-pyenv)
     ((which-function projectile-root) :separator " @ ")
     ((minor-modes :separator spaceline-minor-modes-separator) :when active))
   ;; Right segment (the unimportant stuff)
   '((version-control :when active)))

  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-dimitern)))))

;; powerline: the power-horse of spaceline.
(use-package powerline
  :ensure t
  :after spaceline-config
  :config (validate-setq
           powerline-height (truncate (* 1.0 (frame-char-height)))
           powerline-default-separator 'utf-8))

;; which-key: show help popups for prefix keys.
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
    "C-c 8 >" "typo/right-brackets"
    ;; Prefixes for my personal bindings
    "C-c a" "applications"
    "C-c b" "buffers"
    "C-c c" "compile-and-comments"
    "C-c e" "errors"
    "C-c f" "files"
    "C-c f v" "variables"
    "C-c g" "git"
    "C-c g g" "github/gist"
    "C-c h" "help"
    "C-c i" "insert"
    "C-c i l" "licenses"
    "C-c j" "jump"
    "C-c l" "language/spelling"
    "C-c m" "major mode"
    "C-c o" "cursors"
    "C-c o i" "cursors/insert"
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

  :diminish which-key-mode)

;; which-func: show the current elisp function in mode line.
(use-package which-func
  :defer 1
  :config
  (which-function-mode)
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

;; Show current line/column number in mode-line.
(line-number-mode)
(column-number-mode)

(provide 'dimitern-mode-line)
