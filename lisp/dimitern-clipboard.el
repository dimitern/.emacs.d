;; dimitern-clipboard.el: clipboard related config.
;;

(validate-setq
 ;; Save existing clipboard contents to kill ring before killing.
 save-interprogram-paste-before-kill t
 ;; Transfer clipboard back to X before quitting Emacs.
 x-select-enable-clipboard-manager t
 ;; Use X clipboard and primary selection (with the mouse).
 x-select-enable-clipboard t
 x-select-enable-primary t
 ;; Ignore ring-bell, middle-click paste.
 ring-bell-function '(lambda ())
 ;; Paste with mouse-2 at point, not where I clicked.
 mouse-yank-at-point t
 )

(provide 'dimitern-clipboard)
