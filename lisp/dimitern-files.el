;; dimitern-files.el: global files-related config.
;;

;; files: core commands for files.
(use-package files
  :bind
  (("C-c f z" . revert-buffer)
   ("C-c f /" . revert-buffer)
   ;; Additional bindings for built-ins
   ("C-c f v d" . add-dir-local-variable)
   ("C-c f v l" . add-file-local-variable)
   ("C-c f v p" . add-file-local-variable-prop-line)))

;; ffap: find files at point.
(use-package ffap
  :defer t
  :bind (("C-c C-f" . find-file-at-point)
         ("C-c C-d" . dired-at-point))
  ;; Please stop pinging random hosts!  See
  ;; https://github.com/technomancy/emacs-starter-kit/issues/39
  :config (setq ffap-machine-p-known 'reject))

;; server: the server of `emacsclient'.
(use-package server
  :if (not noninteractive)
  :defer t
  :config
  (unless (server-running-p server-name)
    (server-mode))
  :diminish (server-buffer-clients . "ⓒ"))

;; dired: edit directories.
(use-package dired
  :defer t
  :config
  (validate-setq
   dired-auto-revert-buffer t           ; Revert on re-visiting
   ;; Better dired flags: `-l' is mandatory, `-a' shows all files, `-h' uses
   ;; human-readable sizes, and `-F' appends file-type classifiers to file names
   ;; (for better highlighting)
   dired-listing-switches "-alhF"
   dired-ls-F-marks-symlinks t          ; -F marks links with @
   ;; Inhibit prompts for simple recursive operations
   dired-recursive-copies 'always
   ;; Auto-copy to other Dired split window
   dired-dwim-target t)

  (when (or (memq system-type '(gnu gnu/linux))
            (string= (file-name-nondirectory insert-directory-program) "gls"))
    ;; If we are on a GNU system or have GNU ls, add some more `ls' switches:
    ;; `--group-directories-first' lists directories before files, and `-v'
    ;; sorts numbers in file names naturally, i.e. "image1" goes before
    ;; "image02"
    (validate-setq
     dired-listing-switches
     (concat dired-listing-switches " --group-directories-first -v"))))

;; dired-x: additional tools for dired.
(use-package dired-x
  :defer t
  :bind (("C-c f j" . dired-jump)
         ("C-x C-j" . dired-jump))
  :init
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  :after dired
  :config
  (validate-setq dired-omit-verbose nil)        ; Shut up, dired

  (when (dimitern-os/is-darwin)
    ;; OS X bsdtar is mostly compatible with GNU Tar
    (validate-setq dired-guess-shell-gnutar "tar"))

  ;; Diminish dired-omit-mode. We need this hack, because Dired Omit Mode has
  ;; a very peculiar way of registering its lighter explicitly in
  ;; `dired-omit-startup'.  We can't just use `:diminish' because the lighter
  ;; isn't there yet after dired-omit-mode is loaded.
  (add-function :after (symbol-function 'dired-omit-startup)
                (lambda () (diminish 'dired-omit-mode "ⓞ"))
                '((name . dired-omit-mode-diminish))))

;; open neotree at projectile project root and select current file.
;; Source: https://www.emacswiki.org/emacs/NeoTree
(defun dimitern/neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

;; ignoramus: ignore uninteresting files everywhere.
(use-package ignoramus
  :ensure t
  :config
  ;; Ignore some additional directories and file extensions
  (dolist (name '("company-statistics-cache.el"
                  ".cask"
                  ".vagrant"
                  ".ensime_cache" ".ensime"
                  ".stack-work"))
    ;; Ignore some additional directories
    (add-to-list 'ignoramus-file-basename-exact-names name))

  (dolist (ext '(".fls" ".out" ; LaTeX
                 ))
    (add-to-list 'ignoramus-file-endings ext))

  (ignoramus-setup))

;; hardhat: protect user-writable files.
(use-package hardhat
  :ensure t
  :init (global-hardhat-mode)
  :config (validate-setq hardhat-mode-lighter "⚠"))

;; bookmarks: bookmarks for Emacs buffers.
(use-package bookmark
  :bind (("C-c f b" . list-bookmarks))
  ;; Save bookmarks immediately after a bookmark was added
  :config (validate-setq bookmark-save-flag 1))

;; recentf: save recently visited files.
(use-package recentf
  :init (recentf-mode)
  :config
  (validate-setq
   recentf-max-saved-items 200
   recentf-max-menu-items 15
   ;; Cleanup recent files only when Emacs is idle, but not when the mode
   ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
   ;; idles often enough to have the recent files list clean up regularly
   recentf-auto-cleanup 300
   recentf-exclude (list "/\\.git/.*\\'"     ; Git contents
                         "/elpa/.*\\'"       ; Package files
                         "/itsalltext/"      ; It's all text temp files
                         ;; And all other kinds of boring files
                         #'ignoramus-boring-p)))

;; saveplace: save point position in files.
(use-package saveplace
  :init (save-place-mode 1))

;; view read-only files.
(validate-setq view-read-only t)

;; autorevert: auto-revert buffers of changed files.
(use-package autorevert
  :init (global-auto-revert-mode)
  :bind (("C-c t a" . auto-revert-mode))
  :config
  (validate-setq auto-revert-verbose nil ; Shut up, please!
                 ;; Revert Dired buffers, too
                 global-auto-revert-non-file-buffers t)

  (when (dimitern-os/is-darwin)
    ;; File notifications aren't supported on OS X
    (validate-setq auto-revert-use-notify nil))
  :diminish (auto-revert-mode . "🔃"))

;; image-file: visit images as images.
(use-package image-file
  :init (auto-image-file-mode))

;; launch: open files in external programs
(use-package launch
  :ensure t
  :defer t)

;; sudo-edit: edit files as root, through Tramp
(use-package sudo-edit
  :ensure t
  :defer t
  :bind (("C-c f s" . sudo-edit)))

;; reveal-in-osx-finder: reveal current buffer in finder.
(use-package reveal-in-osx-finder
  :when (dimitern-os/is-darwin)
  :ensure t
  ;; Bind analogous to `dired-jump' at C-c f j
  :bind (("C-c f J" . reveal-in-osx-finder)))

;; expand-region:eExpand region by semantic units
(use-package expand-region
  :ensure t
  :bind (("C-c v" . er/expand-region)))

;; undo-tree: branching undo.
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode)

;; Give us narrowing back!
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Same for region casing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Additional keybindings
(bind-key [remap just-one-space] #'cycle-spacing)
(bind-key "C-c SPC" #'cycle-spacing)

;;; Internationalisation
(prefer-coding-system 'utf-8)

;; mule-cmds: input methods.
(use-package mule-cmds
  :defer t
  :bind (("C-c t 8" . toggle-input-method))
  :config
  (validate-setq default-input-method "german-postfix"))

;; smartparens: parenthesis editing and balancing.
(use-package smartparens
  :ensure t
  :bind (("C-c k" . dimitern-smartparens/body)
         :map smartparens-strict-mode-map
         ;; A fill paragraph in strict mode
         ("M-q" . sp-indent-defun))
  :init
  ;; Hydra for Smartparens
  (defhydra dimitern-smartparens (:hint nil)
    "
Sexps (quit with _q_)
^Nav^            ^Barf/Slurp^                 ^Depth^
^---^------------^----------^-----------------^-----^-----------------
_f_: forward     _→_:          slurp forward   _R_: splice
_b_: backward    _←_:          barf forward    _r_: raise
_u_: backward ↑  _C-<right>_:  slurp backward  _↑_: raise backward
_d_: forward ↓   _C-<left>_:   barf backward   _↓_: raise forward
_p_: backward ↓
_n_: forward ↑
^Kill^           ^Misc^                       ^Wrap^
^----^-----------^----^-----------------------^----^------------------
_w_: copy        _j_: join                    _(_: wrap with ( )
_k_: kill        _s_: split                   _{_: wrap with { }
^^               _t_: transpose               _'_: wrap with ' '
^^               _c_: convolute               _\"_: wrap with \" \"
^^               _i_: indent defun"
    ("q" nil)
    ;; Wrapping
    ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))
    ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))
    ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")))
    ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")))
    ;; Navigation
    ("f" sp-forward-sexp )
    ("b" sp-backward-sexp)
    ("u" sp-backward-up-sexp)
    ("d" sp-down-sexp)
    ("p" sp-backward-down-sexp)
    ("n" sp-up-sexp)
    ;; Kill/copy
    ("w" sp-copy-sexp)
    ("k" sp-kill-sexp)
    ;; Misc
    ("t" sp-transpose-sexp)
    ("j" sp-join-sexp)
    ("s" sp-split-sexp)
    ("c" sp-convolute-sexp)
    ("i" sp-indent-defun)
    ;; Depth changing
    ("R" sp-splice-sexp)
    ("r" sp-splice-sexp-killing-around)
    ("<up>" sp-splice-sexp-killing-backward)
    ("<down>" sp-splice-sexp-killing-forward)
    ;; Barfing/slurping
    ("<right>" sp-forward-slurp-sexp)
    ("<left>" sp-forward-barf-sexp)
    ("C-<left>" sp-backward-barf-sexp)
    ("C-<right>" sp-backward-slurp-sexp))

  (show-smartparens-mode t)
  (add-hook 'nxml-mode-hook #'turn-off-show-smartparens-mode)

  (dolist (hook '(inferior-emacs-lisp-mode-hook
                  emacs-lisp-mode-hook))
    (add-hook hook #'smartparens-strict-mode))
  :config
  (require 'smartparens-config)

  (validate-setq sp-autoskip-closing-pair 'always
                 ;; Don't kill entire symbol on C-k
                 sp-hybrid-kill-entire-symbol nil)
  :diminish (smartparens-mode . "⒫"))

(defun dimitern-whitespace-style-no-long-lines ()
  "Configure `whitespace-mode' for Org.
Disable the highlighting of overlong lines."
  (setq-local whitespace-style (-difference whitespace-style
                                            '(lines lines-tail))))

(defun dimitern-whitespace-mode-local ()
  "Enable `whitespace-mode' after local variables where set up."
  (add-hook 'hack-local-variables-hook #'whitespace-mode nil 'local))

;; whitespace: highlight bad whitespace.
(use-package whitespace
  :bind (("C-c t w" . whitespace-mode))
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'dimitern-whitespace-mode-local))
  :config
  ;; Highlight tabs, empty lines at beg/end, trailing whitespaces and overlong
  ;; portions of lines via faces.  Also indicate tabs via characters
  (validate-setq
   whitespace-style '(face indentation space-after-tab space-before-tab
                           tab-mark empty trailing lines-tail)
   whitespace-line-column 88)
  :diminish (whitespace-mode . "ⓦ"))

;; highlight-numbers: fontify number literals.
(use-package highlight-numbers
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; rainbow-mode: fontify color values in code
(use-package rainbow-mode
  :ensure t
  :pin "gnu"
  :bind (("C-c t r" . rainbow-mode))
  :config (add-hook 'css-mode-hook #'rainbow-mode))

;; rainbow-delimiters: highlight delimiters by depth.
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :config
  (rainbow-delimiters-mode-disable)
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

;; highlight-symbol: highlighting and commands for symbols.
(use-package highlight-symbol
  :ensure t
  :defer t
  :bind
  (("C-c s %" . highlight-symbol-query-replace)
   ("C-c s n" . highlight-symbol-next-in-defun)
   ("C-c s p" . highlight-symbol-prev-in-defun)
   ("C-c s o" . highlight-symbol-occur))
  ;; Navigate occurrences of the symbol under point with M-n and M-p, and
  ;; highlight symbol occurrences
  :init
  (dolist (fn '(highlight-symbol-nav-mode highlight-symbol-mode))
    (add-hook 'prog-mode-hook fn))
  :config
  (validate-setq
   highlight-symbol-idle-delay 0.4          ; Highlight almost immediately
   highlight-symbol-on-navigation-p t)      ; Highlight immediately after
                                        ; navigation
  :diminish highlight-symbol-mode)

;; hl-todo: highlight TODOs in buffers.
(use-package hl-todo
  :ensure t
  :defer t
  :init (global-hl-todo-mode))

;; auto-insert: automatic insertion into new files.
(use-package auto-insert
  :defer t
  :bind (("C-c i a" . auto-insert)))

;; ;; ispell: spell checking.
;; (use-package ispell
;;   :defer t
;;   :config
;;   (validate-setq
;;    ispell-program-name (executable-find "ispell")
;;    ispell-silently-savep t              ; Don't ask when saving the private dict
;;    ;; Increase the height of the choices window to take our header line
;;    ;; into account.
;;    ispell-choices-win-default-height 5)

;;   (unless ispell-program-name
;;     (warn "No spell checker available.  Install Hunspell or ASpell for OS X.")))

;; flyspell: on-the-fly spell checking.
(use-package flyspell
  :bind (("C-c t s" . flyspell-mode)
         ("C-c M-l b" . flyspell-buffer))
  :init
  (dolist (hook '(text-mode-hook message-mode-hook))
    (add-hook hook 'turn-on-flyspell))
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  :config
  (validate-setq
   flyspell-use-meta-tab nil
   ;; Make Flyspell less chatty
   flyspell-issue-welcome-flag nil
   flyspell-issue-message-flag nil)

  ;; Free C-M-i for completion
  (define-key flyspell-mode-map "\M-\t" nil)
  ;; Undefine mouse buttons which get in the way
  (define-key flyspell-mouse-map [down-mouse-2] nil)
  (define-key flyspell-mouse-map [mouse-2] nil)
  :diminish (flyspell-mode . "ⓢ"))

;; auto-dictionary: automatically infer dictionary.
(use-package auto-dictionary
  :ensure t
  ;; Always change dictionary through adict, because it triggers hooks that let
  ;; us automatically update the "language" for other modes (e.g. Typo Mode) as
  ;; well
  :bind (("C-c M-l l" . adict-change-dictionary)
         ("C-c M-l g" . adict-guess-dictionary))
  :diminish (auto-dictionary-mode)
  :init
  (add-hook 'flyspell-mode-hook #'auto-dictionary-mode))

;; flycheck: on-the-fly syntax checking.
(use-package flycheck
  :ensure t
  :defer 1
  :bind (("C-c t f" . flycheck-mode))
  :config
  (defhydra dimitern-flycheck-errors ()
    "Flycheck errors."
    ("n" flycheck-next-error "next")
    ("p" flycheck-previous-error "previous")
    ("f" flycheck-first-error "first")
    ("l" flycheck-list-errors "list")
    ("w" flycheck-copy-errors-as-kill "copy message"))
  (setq-default flycheck-flake8-maximum-line-length 120)

  (global-flycheck-mode)
  (validate-setq
   flycheck-display-errors-function (lambda (errors)
                                      (progn
                                        (flycheck-display-error-messages-unless-error-list errors)
                                        (dimitern-flycheck-errors/body)))
   flycheck-scalastylerc "scalastyle_config.xml")
  :diminish (flycheck-mode . "🗹"))

;; flycheck-pos-tip: show Flycheck errors in tooltip.
(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config (flycheck-pos-tip-mode))

;; flycheck-title: show Flycheck errors in frame title.
(use-package flycheck-title
  :ensure t
  :after flycheck
  :config (flycheck-title-mode))

;;; Internationalisation
(prefer-coding-system 'utf-8)

;; dockerfile - major mode for editing Dockerfiles.
(use-package dockerfile-mode
  :pin "melpa"
  :ensure t
  :mode ("\\Dockerfile\\'" . dockerfile-mode))

(use-package docker-compose-mode
  :pin "melpa"
  :ensure t
  :bind (("C-c d c" . docker-compose-mode))
  :diminish (docker-compose-mode . "⛭")
  :mode ("\\docker-compose\\.ya?ml\\'" . docker-compose-mode))

;; docker - Emacs interface to Docker.
(use-package docker
  :pin "melpa"
  :bind (("C-c d d" . docker))
  :diminish (docker-mode . "🐋")
  :ensure t)

;; docker-cli - Emacs interface to Docker CLI.
(use-package docker-cli
  :pin "melpa"
  :bind (("C-c d r" . docker-cli))
  :ensure t)

(provide 'dimitern-files)
