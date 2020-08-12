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

;; Golang configuration
(use-package go-mode
  :ensure t
  :defer t
  :config
  (defun dimitern/golang-onsave()
    "Run golang-onsave.sh, which does runs go build, install, test, and vet."
    (interactive)
    (shell-command "~/work/bin/golang-onsave.sh"))

  (defun dimitern/go-coverage-current-file()
    "Run go-coverage, passing 'cover' as the filename, instead of asking"
    (interactive)
    (go-coverage
     (if (boundp 'go--coverage-current-file-name)
         go--coverage-current-file-name
       "cover")))

  ;; gofmt on save using goimports
  (setq gofmt-command "goimports")
  (setq go-test-verbose t)
  (add-hook 'before-save-hook 'gofmt-before-save)

  :bind (("C-c i" . go-goto-imports)
         ("M-." . godef-jump)
         ("M-*" . xref-pop-marker-stack)
         ("C-M-." . godef-jump-other-window)
         ("C-c g h" . dimitern/go-coverage-current-file)
         ("C-c g d" . godoc-at-point)
         ("C-c g c" . go-test-current-coverage)
         ("C-c g t" . go-test-current-test)
         ("C-c g f" . go-test-current-file)
         ("C-c g p" . go-test-current-project)
         ("C-c g x" . go-run)
         ("C-c g l" . golint)
         ("C-c g r" . go-rename)
         ("C-c g SPC" . dimitern/golang-onsave)))

;; go-errcheck - helper to run errcheck.
(use-package go-errcheck
  :pin "melpa"
  :defer t
  :ensure t)

;; When saving files, set execute permission if #! is in first line.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; package-lint: linter for Emacs lisp packages
(use-package package-lint
  :pin "melpa"
  :defer t
  :ensure t)

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
  :defer t
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
  :defer t
  :bind (("C-c t p" . prettify-symbols-mode)))

;; compile: Compile from Emacs.
(use-package compile
  :defer t
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
  :defer t
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

(defun dimitern/toggle-subword-mode ()
  "Toggle between `subword-mode' and `superword-mode' for `python-mode-hook'"
  (interactive)
  (if (memq 'subword-mode python-mode-hook)
      (progn
        (superword-mode)
        (message "Disabled subword-mode for current buffer..."))
    (subword-mode)
    (message "Enabled subword-mode for current buffer...")))

;; pydoc: browse Python documentation.
(use-package pydoc
  :defer t
  :ensure t
  :after anaconda-mode
  :bind (("C-M-." . pydoc-at-point)))

;; ein: Emacs IPython Notebooks.
(use-package ein
  :defer t
  :pin "melpa"
  :ensure t)

;; haml-mode: HAML syntax support.
(use-package haml-mode
  :defer t
  :ensure t
  :mode ("\\.haml?\\'" . haml-mode))

(defun dimitern/isort-blacken-save-buffer ()
  "Calls `py-isort-buffer', `blacken-mode', and `save-buffer'."
  (interactive)
  (progn (py-isort-buffer) (blacken-mode) (save-buffer)))

(defun dimitern/toggle-py-isort-on-save ()
  (interactive)
  (if (memq 'py-isort-before-save before-save-hook)
      (progn
        (remove-hook 'before-save-hook #'py-isort-before-save t)
        (message "Disabled py-isort-before-save for current buffer..."))
    (add-hook 'before-save-hook #'py-isort-before-save  nil t)
    (message "Enabled py-isort-before-save for current buffer...")))

(define-minor-mode py-isort-mode
  "Automatically run `py-isort-buffer' before saving."
  :lighter "ｉ"
  (if (memq 'py-isort-buffer before-save-hook)
      (progn
        (remove-hook 'before-save-hook 'py-isort-buffer t)
        (message "Disabled py-isort-mode for current buffer..."))
    (add-hook 'before-save-hook 'py-isort-buffer nil t)
    (message "Enabled py-isort-mode for current buffer...")))


(defun dimitern/python-hook ()
  "Automatically executed code for Python files."
  (interactive)
  (progn
    ;; Enable subword navigation, and line numbers.
    (subword-mode)
    (nlinum-mode)
    ;; Auto-configure virtualenv for interactive shells, eshell, gud/pdb, flycheck, flake8, pipenv.
    (venv-projectile-auto-workon)
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)
    (dimitern-gud-set-pdb-cmdline)
    (pipenv-mode)
    (flycheck-mode-on-safe)
    (flycheck-set-checker-executable 'python-flake8 "flake8")
    (flycheck-select-checker 'python-flake8)
    ;; Apply black and isort before saving
    (blacken-mode t)
    (py-isort-mode)))

;; python: Python editing.
(use-package python
  :ensure t
  :defer 2
  :mode ("\\.pyw?\\'" . python-mode)
  :bind (("M-RET" . dimitern-pdb/body)
         ;;("M-/" . company-complete-common-or-cycle)
         ("<tab>" . indent-according-to-mode)
         ("<backtab>" . indent-rigidly)
         ("C-c t s" . dimitern/toggle-subword-mode)
         ("C-c f F" . dimitern/isort-blacken-save-buffer)
         ("C-<tab>" . anaconda-mode-complete))
  :config
  (validate-setq
   python-indent-offset 4
   indent-tabs-mode nil)

  (add-hook 'python-mode-hook 'dimitern/python-hook)

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


;; py-isort: sorts Python imports uniformly.
(use-package py-isort
  :ensure t
  :defer t
  :after python
  :bind (:map python-mode-map
              ("C-c t i" . dimitern/toggle-py-isort-on-save)
              ("C-c f I" . #'py-isort-buffer))

  ;; The following settings make isort compatible with black.
  :config
  (validate-setq
   py-isort-options '("-sp $(pwd)/setup.cfg")))

;; pipenv: Pipenv porcelain inside Emacs
(use-package pipenv
  :pin "melpa"
  :ensure t
  :defer t
  :after python
  :diminish (pipenv-mode . "󠁐🅿")
  :config
  (validate-setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-default))

;; ;; flycheck-mypy: FlyCheck interface to MyPy static type checker.
;; (use-package flycheck-mypy
;;   :ensure t
;;   :after python
;;   :config
;;   (flycheck-add-next-checker 'python-flake8 'python-mypy))

;; projectile: project management for Emacs.
(use-package projectile
  :ensure t
  :config
  (validate-setq projectile-completion-system 'ivy
                 projectile-find-dir-includes-top-level t)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)

  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  :diminish projectile-mode)

(defvar dimitern-virtualenv-workon-home
  (or
   (getenv "WORKON_HOME")
   (if (dimitern-os/is-linux)
       (expand-file-name "~/work/pyenvs/")
     (expand-file-name "z:/pyenvs/")))
  "The $WORKON_HOME path.")

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
  :defer t
  :bind (("C-z" . dimitern-venv/body))
  :after projectile
  :config
   ;; Set venvs location from $WORKON_HOME or directly.
  (venv-set-location dimitern-virtualenv-workon-home)

  (defhydra dimitern-venv (:hint nil)
    "
virtualenvwrapper (quit with _q_)
^Commands^
^--------^------------------------
_a_: projectile auto workon
_w_: workon
_d_: deactivate
_r_: remove
_l_: list
_c_: cd
_m_: make
_p_: copy"
    ("q" nil)
    ("a" #'(lambda () (interactive) (venv-projectile-auto-workon)))
    ("w" venv-workon)
    ("d" venv-deactivate)
    ("r" venv-rmvirtualenv)
    ("l" venv-lsvirtualenv)
    ("c" venv-cdvirtualenv)
    ("m" venv-mkvirtualenv)
    ("p" venv-cpvirtualenv)))

;; anaconda-mode: powerful Python backend for Emacs.
(use-package anaconda-mode
  :ensure t
  :defer t
  :after python
  :config
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode)
  (add-hook 'venv-postactivate-hook
            #'(lambda () (pythonic-activate (format "%s/.venv" (projectile-project-root)))))
  :diminish (anaconda-mode . "🅐"))

;; company-anaconda: Python backend for Company.
(use-package company-anaconda
  :ensure t
  :defer t
  :after anaconda-mode
  :config (add-to-list 'company-backends 'company-anaconda))

;; blacken: Python Black code formatter support.
(use-package blacken
  :pin "melpa"
  :ensure t
  :defer t
  :after python
  :bind (:map python-mode-map
              ("C-c t b" . blacken-mode)
              ("C-c f B" . #'blacken-buffer))
  :diminish (blacken-mode . "🅱"))

;; pip-requirements: requirements.txt files editing.
(use-package pip-requirements
  :ensure t
  :defer t
  :after python)


;; py-isort: sorts Python imports uniformly.
(use-package py-isort
  :ensure t
  :defer t
  :after python)

;; pytest + PDB integration.
(use-package pytest-pdb-break
  :diminish (pytest-pdb-break-mode . "🆃")
  :after pytest
  :defer t
  :ensure t)

;; pytest + PDB integration.
(use-package pytest
  :ensure t
  :defer t
  :after python
  :config
  (validate-setq
   pytest-pdb-break-pytest-executable "pytest"
   pytest-global-name "pytest")
  :bind (:map python-mode-map
              ("C-c C-SPC t" . pytest-one)
              ("C-c C-SPC a" . pytest-all)
              ("C-c C-SPC p" . pytest-pdb-one)
              ("C-c C-SPC d" . pytest-pdb-all)))

;; js-mode: JavaScript major mode.
(use-package js
  :ensure t
  :defer t
  :hook (js-mode . prettier-js-mode)
  :init
  (require 'prettier-js)
  :diminish (js-mode . "Ⓙ")
  :mode ("\\.\\(js\\|jsx\\|htm\\|html\\|hbs\\)\\'" . js-mode))

(defun dimitern/setup-tide-mode ()
  "Setup function for tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (diminish 'prettier-js-mode "Ƥ")
  (setq
   prettier-js-args
   '("--trailing-comma" "none"
     "--bracket-spacing" "true"
     "--single-quote" "true"
     "--no-semi" "true"
     "--jsx-single-quote" "true"
     "--jsx-bracket-same-line" "true"
     "--print-width" "100")))

;; tide: development environment for JavaScript and TypeScript.
(use-package tide
  :ensure t
  :after js
  :defer t
  :pin "melpa"
  :hook (js-mode . dimitern/setup-tide-mode)
  :diminish (tide-mode . "𝓽")
  :config
  ;; prettier-js-mode: JavaScript formatter.
  (setq company-tooltip-align-annotations t))

;; web-mode: HTML editing.
;; (use-package web-mode
;;   :ensure t
;;   :defer t
;;   :config
;;   (validate-setq
;;    web-mode-enable-current-column-highlight t
;;    web-mode-enable-current-element-highlight t
;;    web-mode-markup-indent-offset 2
;;    web-mode-code-indent-offset 2
;;    web-mode-css-indent-offset 2
;;    tab-width 2)
;;   :mode ("\\.\\(jsx?\\|html?\\|hbs\\)\\'" . web-mode))


;; add-node-modules-path: Use tools from node_module, rather than global.
(use-package add-node-modules-path
  :ensure t
  :defer t
  :after web-mode)

;; css-mode: CSS editing.
(use-package css-mode
  :defer t
  :config (validate-setq css-indent-offset 2))

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

  ;; :bind (("C-<right>" . forward-sexp)
  ;;        ("C-<left>"  . backward-sexp)
  ;;        ("C-<down>" . nxml-down-element)
  ;;        ("C-<up>" . nxml-backward-up-element))

  ;; Complete closing tags, and insert XML declarations into empty files
  :config
  (validate-setq nxml-slash-auto-complete-flag t
                 nxml-auto-insert-xml-declaration-flag t
                 line-move-visual t)
  (visual-line-mode 1)
  )

;; holiday: calendar config and holidays.
;; (use-package holiday
;;   :pin "gnu"
;;   :init
;;   (require 'bulgarian-holidays)
;;   (setq
;;    ;; use European date format.
;;    calendar-european-month-header t
;;    ;; local holidays in Bulgaria.
;;    calendar-holidays holiday-bulgarian-holidays
;;    )
;;   )

;; org-mode: Best mode of all!
(use-package org
  :pin "org"
  :ensure t
  :defer t
  :mode ("\\.\\(org\\|org_archive\\)$" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c a"   . org-agenda)
         ("C-c C-a" . org-attach)
         ("C-c t l" . org-toggle-link-display)
         ("C-c n"))
  :config
  (setq
   org-capture-templates
   '(("t" "TODO" entry
      (file+headline (concat org-directory "/newgtd.org") "Задачи")
      "* TODO %^{Brief Description} %^g\n  Добавено: %U\n  %i\n  %a")
     ("a" "APPT" entry
      (file+headline (concat org-directory "/newgtd.org") "Задачи")
      "* APPT %^{Brief Description} %^g\n  Добавено: %U\n  %i\n  %a")
     )
   ;; do not show completed tasks, if scheduled or with deadline.
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-scheduled-if-done t
   ;; always start the agenda on the current day.
   org-agenda-start-on-weekday nil
   ;; agenda shows all days, even if they have no associated tasks.
   org-agenda-show-all-dates t
   ;; show the following 7 days in the agenda.
   org-agenda-ndays 7
   ;; store captured notes in reverse date order (newest on top).
   org-reverse-note-order t
   ;; persist clocks across Emacs sessions.
   org-clock-persist 'history
   )
  (org-clock-persistence-insinuate)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ledger . t)))
  :init
  (require 'ox-md)
  (setq org-directory (expand-file-name "~/Nextcloud/Dropbox/org-home"))
  (setq org-mobile-directory (expand-file-name "~/Nextcloud/Dropbox/MobileOrg"))
  (setq org-default-notes-file (concat org-directory "/newgtd.org"))
  ;; Make windmove work in org-mode:
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)
  (add-hook
   'org-mode-hook (lambda ()
                    ;; nicer bullets for headings.
                    (org-bullets-mode 1)
                    ;; no wrapping long lines.
                    (visual-fill-column-mode -1)
                    ;; automatically export Org-Revel presentations to HTML on save
                    (dimitern/toggle-org-reveal-export-on-save)
                    ;; no line numbers.
                    (nlinum-mode -1))))

;; org-bullets: UTF-8 bullets for org headings.
(use-package org-bullets
  :ensure t
  :defer t)

;; magit: the one and only Git frontend.
(use-package magit
  :ensure t
  :defer t
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

(use-package gnus
  :defer t
  :init
  (setq
   user-email-address "dimiter@naydenov.net"
   smtpmail-smtp-server "mail5.host.bg"
   message-send-mail-function 'smtpmail-send-it)
  :config
  (validate-setq
   user-full-name "Dimiter Naydenov"
   gnus-select-method
   '(nnimap "personal"
            (nnimap-address "mail5.host.bg")
            (nnimap-stream ssl))
   send-mail-function 'smtpmail-send-it
   gnus-message-archive-group "nnimap:Sent")

  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "company"
                        (nnimap-address "hopkins.host.bg")
                        (nnimap-stream ssl))))

;; Rust mode.
(use-package rust-mode
  :pin "melpa"
  :ensure t
  :defer t
  :mode ("\\.rs\\'" . rust-mode))

;; Rust auto-completion.
(use-package racer
  :after rust-mode
  :ensure t
  :defer t
  :diminish (racer-mode . "Ⓡ")
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (define-key rust-mode-map (kbd "M-/") #'company-indent-or-complete-common)
  (validate-setq
   company-tooltip-align-annotations t
   racer-rust-src-path (getenv "RUST_SRC_PATH"))
  :pin "melpa")

;; Rust support for flycheck.
(use-package flycheck-rust
  :after rust-mode
  :ensure t
  :defer t
  :config
  (add-hook 'rust-mode-hook #'flycheck-rust-setup)
  :pin "melpa")

;; nginx-mode: editing Nginx config files.
(use-package nginx-mode
  :pin "melpa"
  :ensure t
  :defer t
  :mode ("\\.nginx\\'" . nginx-mode))

;; clojure-mode: Clojure support
(use-package clojure-mode
  :pin "melpa"
  :ensure t
  :defer t
  :mode ("\\.clj\\'" . clojure-mode))

;; inf-clojure: Clojure inferior mode
(use-package inf-clojure
  :pin "melpa"
  :ensure t
  :after clojure-mode
  :defer t)

(provide 'dimitern-modes)
