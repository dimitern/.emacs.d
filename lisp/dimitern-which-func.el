;; dimitern-which-func.el package config.
;;

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

(provide 'dimitern-which-func)
