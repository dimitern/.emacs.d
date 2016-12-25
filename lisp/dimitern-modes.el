;; dimitern-modes.el: Per-mode configurations.
;;

(setq
 ;; Use text mode by default for new buffers.
 ;; Must be without validate-setq to work at startup.
 default-major-mode 'text-mode
 )

(setq-default indent-tabs-mode nil)

(validate-setq
 ;; Default indent 4 spaces per tab, use spaces.
 standard-indent 4
 ;; Double space after sentence, final newline.
 sentence-end-double-space nil
 require-final-newline t
 ;; Set default fill column
 fill-column 80
 ;; Miscellaneous
 auto-mode-alist (cons '("\\.mm$" . c++-mode) auto-mode-alist)
 vc-follow-symlinks t
 )

;; When saving files, set execute permission if #! is in first line.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :defer t
  :config
  (add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
  :mode "\\.ya?ml\\'")

;; rst: ReStructuredText.
(use-package rst
  :defer t
  :config
  ;; Indent with 3 spaces after all kinds of literal blocks
  (validate-setq rst-indent-literal-minimized 3
                 rst-indent-literal-normal 3)

  (bind-key "C-=" nil rst-mode-map)
  ;; For similarity with AUCTeX
  (bind-key "C-c C-j" #'rst-insert-list rst-mode-map)
  ;; …and with Markdown Mode
  (bind-key "M-RET" #'rst-insert-list rst-mode-map))

;; markdown-mode
(use-package markdown-mode
  :ensure t
  ;; Just no, dear Markdown Mode.  Don't force that bastard Github dialect upon
  ;; me!
  :mode ("\\.md\\'" . markdown-mode)
  :config
  ;; No filling in GFM, because line breaks are significant.
  (add-hook 'gfm-mode-hook #'turn-off-auto-fill)
  ;; Use visual lines instead
  (add-hook 'gfm-mode-hook #'visual-line-mode)

  (bind-key "C-c C-s C" #'markdown-insert-gfm-code-block markdown-mode-map)
  (bind-key "C-c C-s P" #'markdown-insert-gfm-code-block markdown-mode-map)

  ;; Fight my habit of constantly pressing M-q.  We should not fill in GFM
  ;; Mode.
  (bind-key "M-q" #'ignore gfm-mode-map))

;; json-mode: JSON files.
(use-package json-mode
  :ensure t
  :defer t
  :config
  (add-hook 'json-mode-hook
            ;; Fix JSON mode indentation
            (lambda () (setq-local js-indent-level 4))))

;; json-reformat: Reformat JSON.
(use-package json-reformat
  :ensure t
  :defer t
  :bind (("C-c x j" . json-reformat-region)))

;; graphviz-dot-mode: Graphviz.
(use-package graphviz-dot-mode
  :ensure t
  :defer t
  :config
  (validate-setq graphviz-dot-indent-width 4))

;; prog-mode
(use-package prog-mode
  :bind (("C-c t p" . prettify-symbols-mode)))

;; compile: Compile from Emacs.
(use-package compile
  :bind (("C-c c C" . recompile))
  :config
  (validate-setq
   compilation-ask-about-save nil
   ;; Kill old compilation processes before starting new ones,
   compilation-always-kill t
   ;; Automatically scroll
   compilation-scroll-output 'first-error
   ;; Skip over warnings and info messages in compilation
   compilation-skip-threshold 2
   ;; Don't freeze when process reads from stdin
   compilation-disable-input t
   ;; Show three lines of context around the current message
   compilation-context-lines 3)

  (require 'ansi-color)

  (defun dimitern-colorize-compilation-buffer ()
    "Colorize a compilation mode buffer.
Taken from http://stackoverflow.com/a/3072831/355252."
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max))))

  (add-hook 'compilation-filter-hook #'dimitern-colorize-compilation-buffer))

;; elide-head: elide lengthy GPL headers.
(use-package elide-head
  :bind (("C-c t e" . elide-head))
  :init (add-hook 'prog-mode-hook #'elide-head))

;; eldoc: documentation in minibuffer.
(use-package eldoc
  :defer t
  ;; Enable Eldoc for `eval-expression', too
  :init (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  :config
  (setq-default eldoc-documentation-function #'describe-char-eldoc)
  :diminish (eldoc-mode . "ⓓ"))

;; etags: tag navigation.
(use-package etags
  :defer t
  :config
  ;; Do not query before reverting TAGS tables
  (validate-setq tags-revert-without-query t))

;; elisp-mode: Emacs Lisp editing.
(use-package elisp-mode
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :config
  (setq indent-tabs-mode nil)
  :bind (:map emacs-lisp-mode-map
              ("C-c m e r" . eval-region)
              ("C-c t d" . toggle-debug-on-error)
              ("C-c m e b" . eval-buffer)
              ("C-c m e e" . eval-last-sexp)
              ("C-c m e f" . eval-defun)))

(defun dimitern/newline-and-enter-sexp (&rest ignored)
  "Open a new parenthesis, brace, or bracket with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

;; python: Python editing.
(use-package python
  :ensure t
  :mode ("\\.pyw?\\'" . python-mode)
  :bind (("M-RET" . dimitern-pdb/body))
  :config
  (validate-setq
   python-indent-offset 4
   indent-tabs-mode nil)

  ;; PEP 8 compliant filling rules, 79 chars maximum
  (add-hook 'python-mode-hook (lambda () (validate-setq fill-column 79)))
  (add-hook 'python-mode-hook #'subword-mode)

  (dolist (open-pair '("(" "[" "{"))
  (sp-local-pair 'python-mode open-pair nil
                 :post-handlers '((dimitern/newline-and-enter-sexp "RET"))))

  (defhydra dimitern-pdb (:hint nil)
    "
^Pdb^ (_q_ to quit)
^---^-------------------
_._: start gud/pdb
_b_: set breakpoint
_d_: remove breakpoint
_s_: step into
_n_: step over (next)
_f_: step out (finish)
_c_: continue (run)
_p_: print expression
_e_: exec statement
_<_: up frame
_>_: down frame
_k_: stop subjob
_l_: refresh
"
    ("q" nil)
    ("." pdb)
    ("b" gud-break)
    ("d" gud-remove)
    ("s" gud-step)
    ("n" gud-next)
    ("f" gud-finish)
    ("c" gud-cont)
    ("p" gud-print)
    ("e" gud-statement)
    ("<" gud-up)
    (">" gud-down)
    ("k" gud-stop-subjob)
    ("l" gud-refresh)))

;; projectile: project management for Emacs.
(use-package projectile
  :ensure t
  :defer 1
  :config
  (validate-setq projectile-completion-system 'ivy
                 projectile-find-dir-includes-top-level t)

  (projectile-mode)

  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (bind-key "C-c p TAB" #'dimitern-neotree-project-root)
  (bind-key "C-c p <insert>" #'projectile-add-known-project)

  :diminish projectile-mode)

(defvar dimitern-virtualenv-workon-home
  (or
   (getenv "WORKON_HOME")
   (expand-file-name "~/work/pyenvs/"))
  "The $WORKON_HOME path.")

(defun dimitern-venv-projectile-auto-workon ()
  "If a venv matching the projectile project name exists, switch
to the venv and active it."
  (let ((path
         (concat
          venv-location
          (projectile-project-name)
          (f-path-separator))))
    (if (file-exists-p path)
        (progn
          (setq venv-current-name (projectile-project-name))
          (venv--activate-dir path)
          (message
           "Activated virtualenv `%s' (at `%s')"
           venv-current-name
           path))
      (message
       "No virtualenv (at `%s') matching current project (`%s') to activate."
       path
       (projectile-project-name)))))

(defun dimitern-gud-set-pdb-cmdline ()
  "Set the GUD's pdb command line, followed by the current buffer
filename. Prefer `ipython', if available."
  (let ((python-executable
         (or
          (executable-find "ipython")   ; prefer ipython, if available.
          (executable-find "python")))) ; fallback to python otherwise.
    (if (not (file-exists-p python-executable))
        (error
         (format
          "Neither `ipython' nor `python' executable found in `%s'" exec-path))
      (message "Using `%s' as python executable" python-executable)
      (setq pdb-cmdline (intern (format "%s -m pdb" python-executable))
              gud-pdb-command-name (symbol-name pdb-cmdline))
      ;; Ensure pdb is called with a sensible filename.
      (defadvice pdb (before gud-query-cmdline activate)
        "Provide a better default command line when called interactively."
        (interactive
         (list
          (gud-query-cmdline
           pdb-cmdline
           (file-name-nondirectory buffer-file-name))))))))

;; virtualenvwrapper: emulator for Doug Hellmann's virtualenvwrapper.sh.
(use-package virtualenvwrapper
  :ensure t
  :init
  (defhydra dimitern-venv (:hint nil)
    "
virtualenvwrapper (quit with _q_)
^Commands^
^--------^------------------------
_w_: workon
_d_: deactivate
_r_: remove
_l_: list
_c_: cd
_m_: make
_p_: copy"
    ("q" nil)
    ("w" venv-workon)
    ("d" venv-deactivate)
    ("r" venv-rmvirtualenv)
    ("l" venv-lsvirtualenv)
    ("c" venv-cdvirtualenv)
    ("m" venv-mkvirtualenv)
    ("p" venv-cpvirtualenv))
  :after (python projectile)
  :bind (("C-z" . dimitern-venv/body))
  :config

  (validate-setq
   ;; Set venvs location from $WORKON_HOME or directly.
   venv-location dimitern-virtualenv-workon-home)

  ;; Enable for interactive shells and eshell.
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)

  ;; Active matching venvs when switching to projects.
  (setq
   projectile-switch-project-action
   '(lambda ()
      (dimitern-venv-projectile-auto-workon)
      ;; Fix pdb command line.
      (dimitern-gud-set-pdb-cmdline)
      (projectile-recentf))))

(defun dimitern-neotree-project-root (&optional directory)
  "Open a NeoTree browser for a project DIRECTORY."
  (interactive)
  (let ((default-directory (or directory default-directory)))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (neotree-find (projectile-project-root)))))

;; anaconda-mode: powerful Python backend for Emacs.
(use-package anaconda-mode
  :ensure t
  :after virtualenvwrapper
  :config
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode)
  (add-hook 'venv-postactivate-hook
            #'(lambda () (pythonic-activate venv-current-dir)))
  :diminish (anaconda-mode . "Ⓐ"))

;; company-anaconda: Python backend for Company.
(use-package company-anaconda
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-anaconda))

;; pip-requirements: requirements.txt files editing.
(use-package pip-requirements
  :ensure t
  :defer t)

;; web-mode: HTML editing.
(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.html?\\'" . web-mode))

;; css-mode: CSS editing.
(use-package css-mode
  :defer t
  :config (validate-setq css-indent-offset 2))

;; js2-mode: powerful Javascript mode.
(use-package js2-mode
  :ensure t
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :config
  ;; Disable parser errors and strict warnings.  We have Flycheck 8)
  (validate-setq js2-mode-show-parse-errors nil
                 js2-mode-show-strict-warnings nil
                 js2-highlight-level 3  ; Try to highlight most ECMA built-ins
                 ))

;; sh-script: shell scripts.
(use-package sh-script
  :defer t
  :mode ("\\.z?sh\\'" . sh-mode)
  :config
  ;; Use two spaces in shell scripts.
  (validate-setq
   sh-indentation 4                     ; The basic indentation
   sh-basic-offset 4                    ; The offset for nested indentation
   ))

;; nxml-mode: XML editing.
(use-package nxml-mode
  :defer t
  ;; Complete closing tags, and insert XML declarations into empty files
  :config (validate-setq nxml-slash-auto-complete-flag t
                         nxml-auto-insert-xml-declaration-flag t))

;; org-mode: Best mode of all!
(use-package org-mode
  :mode ("\\.org\\'" . org-mode)
  :defer t
  :bind (("C-c C-a" . org-agenda)
         ("C-c o a" . org-attach)
         ("C-c o o" . org-agenda)
         ("C-c o b" . org-iswitchb)
         ("C-c o l" . org-store-link)
         :map org-mode-map
         ("C-c C-a" . org-agenda)
         ("C-c a"   . org-attach))
  )

;; org-bullets: UTF-8 bullets for org headings.
(use-package org-bullets
  :ensure t
  :after org-mode
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

;; magit: the one and only Git frontend.
(use-package magit
  :ensure t
  :pin "melpa-stable"
  :bind (("C-c C-g" . magit-status)
         ("C-c g c" . magit-clone)
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-buffer-file)
         ("C-c g p" . magit-pull))
  :config
  ;; Shut up, Magit
  (validate-setq
   magit-completing-read-function #'ivy-completing-read
   magit-save-repository-buffers 'dontask
   magit-refs-show-commit-count 'all
   ;; Use separate buffers for one-file logs so that we don't need to reset
   ;; the filter everytime for full log view
   magit-log-buffer-file-locked t
   ;; This is creepy, Magit
   magit-revision-show-gravatars nil
   ;; Show status buffer in fullscreen
   magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
   )

  ;; Set Magit's repo dirs for `magit-status' from Projectile's known
  ;; projects.  Initialize the `magit-repository-directories'
  ;; immediately after Projectile was loaded, and update it every time
  ;; we switched projects, because the new project might have been
  ;; unknown before
  (defun dimitern-magit-set-repo-dirs-from-projectile ()
    "Set `magit-repo-dirs' from known Projectile projects."
    (let ((project-dirs (bound-and-true-p projectile-known-projects)))
      ;; Remove trailing slashes from project directories, because
      ;; Magit adds trailing slashes again, which breaks the
      ;; presentation in the Magit prompt.
      (validate-setq magit-repository-directories
                     (mapcar #'directory-file-name project-dirs))))

  (with-eval-after-load 'projectile
    (dimitern-magit-set-repo-dirs-from-projectile))

  (add-hook 'projectile-switch-project-hook
            #'dimitern-magit-set-repo-dirs-from-projectile))

;; gitconfig-mode: git configuration mode.
(use-package gitconfig-mode
  :ensure t
  :defer t)

;; gitignore-mode: .gitignore mode.
(use-package gitignore-mode
  :ensure t
  :defer t)

;; gitattributes-mode: git attributes mode.
(use-package gitattributes-mode
  :ensure t
  :defer t)

(provide 'dimitern-modes)
