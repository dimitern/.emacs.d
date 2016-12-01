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
  :diminish (eldoc-mode . " ⓓ"))

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

;; We can safely declare this function, since we'll only call it in Python
;; Mode, that is, when python.el was already loaded.
(declare-function python-shell-calculate-exec-path "python")

(defun flycheck-virtualenv-executable-find (executable)
  "Find an EXECUTABLE in the current virtualenv if any."
  (if (bound-and-true-p python-shell-virtualenv-root)
      (let ((exec-path (python-shell-calculate-exec-path)))
        (executable-find executable))
    (executable-find executable)))

(defun flycheck-virtualenv-setup ()
  "Setup Flycheck for the current virtualenv."
  (setq flycheck-executable-find #'flycheck-virtualenv-executable-find))

;; python: Python editing.
(use-package python
  :ensure t
  :mode ("\\.pyw?\\'" . python-mode)
  :config

  (let ((ipython (executable-find "ipython")))
    (if ipython
        (validate-setq python-shell-interpreter ipython)
      (warn "IPython is missing, falling back to default python")))

  (setq
   python-indent-offset 4
   indent-tabs-mode nil)

  ;; PEP 8 compliant filling rules, 79 chars maximum
  (add-hook 'python-mode-hook (lambda () (validate-setq fill-column 79)))
  (add-hook 'python-mode-hook #'subword-mode)
  (add-hook 'python-mode-hook #'dimitern-virtualenv-init-from-workon-home)
  (add-hook 'python-mode-hook #'flycheck-virtualenv-setup))

(defvar dimitern-virtualenv-workon-home
  (or (getenv "WORKON_HOME") (expand-file-name "~/work/pyenvs"))
  "The $WORKON_HOME path.")

(defun dimitern-virtualenv-init-from-workon-home ()
  "Set the current virtualenv for this buffer."
  (let* ((name (projectile-project-name))
         (venv-dir (expand-file-name name dimitern-virtualenv-workon-home)))
    (when (file-directory-p venv-dir)
      (setq python-shell-virtualenv-root venv-dir))))

(defun dimitern-neotree-project-root (&optional directory)
  "Open a NeoTree browser for a project DIRECTORY."
  (interactive)
  (let ((default-directory (or directory default-directory)))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (neotree-find (projectile-project-root)))))

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

;; anaconda-mode: powerful Python backend for Emacs.
(use-package anaconda-mode
  :ensure t
  :defer t
  :init (add-hook 'python-mode-hook #'anaconda-mode)
  :diminish (anaconda-mode . " Ⓐ "))

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

(provide 'dimitern-modes)
