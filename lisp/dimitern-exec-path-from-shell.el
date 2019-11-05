;; dimitern-exec-path-from-shell.el package config.
;;

(use-package exec-path-from-shell
  :ensure t
  :defer 1
  :when (dimitern-os/is-linux)
  :config
  (validate-setq
   exec-path-from-shell-variables '("PYTHONPATH"       ; Python modules
                                    "VIRTUAL_ENV"      ; Current virtualenv
                                    "PIPENV_VENV_IN_PROJECT" ; Pipenv: No .venv in projects
                                    "PIPENV_MAX_DEPTH"       ; Pipenv: Pipfile search dept
                                    "WORKON_HOME"      ; virtualenvwrapper home
                                    "PATH"             ; Executables
                                    "GOPATH"           ; Go packages
                                    "GOROOT"           ; Go itself
                                    "JAVA_OPTS"        ; Options for java processes
                                    "RUST_SRC_PATH"    ; Rust sources (for Racer)
                                    "EMAIL"            ; My personal email
                                    "INFOPATH"         ; Info directories
                                    "MANPATH"          ; Man pages
                                    )
   exec-path-from-shell-check-startup-files nil  ; No warnings (startup speeds up as well)
   )

  ;; Initialize Emacs' environment from the shell
  (exec-path-from-shell-initialize)

  ;; Re-initialize the `Info-directory-list' from $INFOPATH.  Since package.el
  ;; already initializes info, we need to explicitly add the $INFOPATH
  ;; directories to `Info-directory-list'.  We reverse the list of info paths
  ;; to prepend them in proper order subsequently
  (with-eval-after-load 'info
    (dolist (dir (nreverse (parse-colon-path (getenv "INFOPATH"))))
      (when dir
        (add-to-list 'Info-directory-list dir)))))

(provide 'dimitern-exec-path-from-shell)
