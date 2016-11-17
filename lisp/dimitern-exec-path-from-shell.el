;; dimitern-exec-path-from-shell.el package config.
;;

(use-package exec-path-from-shell
  :ensure t
  :config
  (progn
    (validate-setq exec-path-from-shell-variables
                   '("PYTHONPATH"       ; Python modules
		     "GOPATH"           ; Go packages
                     "JAVA_OPTS"        ; Options for java processes
                     "EMAIL"            ; My personal email
                     "PATH"             ; Executables
                     "INFOPATH"         ; Info directories
                     "MANPATH"          ; Man pages
                     ))

    ;; Initialize Emacs' environment from the shell
    (exec-path-from-shell-initialize)

    ;; Re-initialize the `Info-directory-list' from $INFOPATH.  Since package.el
    ;; already initializes info, we need to explicitly add the $INFOPATH
    ;; directories to `Info-directory-list'.  We reverse the list of info paths
    ;; to prepend them in proper order subsequently
    (with-eval-after-load 'info
      (dolist (dir (nreverse (parse-colon-path (getenv "INFOPATH"))))
        (when dir
          (add-to-list 'Info-directory-list dir))))))

(provide 'dimitern-exec-path-from-shell)
