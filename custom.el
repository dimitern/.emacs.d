(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anaconda-mode-eldoc-as-single-line nil)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238"))
 '(calendar-christian-all-holidays-flag t)
 '(calendar-mark-holidays-flag t)
 '(column-number-mode t)
 '(company-abort-manual-when-too-short t)
 '(company-anaconda-case-insensitive nil)
 '(company-backends
   (quote
    (company-racer company-anaconda company-math-symbols-latex company-math-symbols-unicode company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                   (company-dabbrev-code company-gtags company-etags company-keywords)
                   company-oddmuse company-dabbrev)))
 '(company-dabbrev-code-everywhere t)
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-other-buffers t)
 '(company-frontends
   (quote
    (company-pseudo-tooltip-frontend company-echo-metadata-frontend company-preview-if-just-one-frontend company-quickhelp-frontend)))
 '(company-idle-delay 0.25)
 '(company-minimum-prefix-length 2)
 '(company-quickhelp-use-propertized-text t)
 '(company-require-match nil)
 '(company-search-regexp-function (quote company-search-words-regexp))
 '(company-selection-wrap-around t)
 '(company-statistics-size 4000)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-flip-when-above t)
 '(company-tooltip-minimum-width 40)
 '(coverage-dir "/home/dimitern/work/upwork/samuel-shabaz/src/website/")
 '(custom-enabled-themes (quote (material)))
 '(custom-safe-themes
   (quote
    ("7e559cd74b715c744ff8cf276a774e4a087ae7065062cb258bc3ef30faecb55b" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "c1ab17435aaac39206b0f34dddda6349d91004c27258ab55301b973b179ddd55" "b63c0298290e6c6aa17d64fbce2c2a83c4b420d73c378f4f96dd8e1883ac6d51" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" default)))
 '(ein:default-url-or-port "http://127.0.0.1:8889")
 '(ein:enable-keepalive t)
 '(ein:url-or-port (quote ("http://127.0.0.1:8889")))
 '(enable-local-variables :safe)
 '(fci-rule-color "#37474f")
 '(flycheck-disabled-checkers (quote (python-pylint python-pycompile)))
 '(flycheck-go-build-executable nil)
 '(flycheck-go-gofmt-executable "gofmt")
 '(flycheck-go-golint-executable "golint")
 '(flycheck-go-vet-executable "govet")
 '(flycheck-python-mypy-executable nil)
 '(frame-background-mode (quote dark))
 '(global-nlinum-mode t)
 '(go-command "$GOROOT/bin/go")
 '(hippie-expand-ignore-buffers (quote ("^ \\*.*\\*$" dired-mode)))
 '(hippie-expand-try-functions-list
   (quote
    (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(hl-sexp-background-color "#1c1f26")
 '(ivy-action-wrap t)
 '(ivy-display-function nil)
 '(ivy-wrap t)
 '(linum-delay nil)
 '(magit-git-debug t t)
 '(mouse-wheel-scroll-amount (quote (0.07)))
 '(neo-show-hidden-files nil)
 '(neo-vc-integration (quote (face char)))
 '(nlinum-highlight-current-line t)
 '(nlinum-use-right-margin t)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org-home/newgtd.org" "~/Dropbox/org-home/birthday.org")))
 '(package-selected-packages
   (quote
    (haml-mode pycoverage ov py-isort nginx-mode nginx company-racer company-rust flycheck-rust racer rust-mode rust flycheck-mypy material-light material ein go-errcheck errcheck go-mode pydoc multiple-cursors package-lint py-autopep8 org-mobile org-mode holiday org-capture org-bullets material-theme gitattributes-mode gitignore-mode gitconfig-mode magit rainbow-delimiters ibuffer-projectile virtualenvwrapper ace-link avy ivy-pages visual-fill-column adaptive-wrap whitespace-cleanup-mode easy-kill counsel-projectile projectile js2-mode web-mode pip-requirements company-anaconda anaconda-mode graphviz-dot-mode json-mode markdown-mode flycheck-title flycheck-pos-tip flycheck auto-dictionary company-emoji company-math company-statistics company-quickhelp company yasnippet rainbow-mode highlight-numbers highlight-symbol hl-todo expand-region undo-tree smartparens nlinum reveal-in-osx-finder sudo-edit launch hardhat ignoramus neotree golden-ratio ace-window ibuffer-vc focus-autosave-mode stripe-buffer beacon page-break-lines yaml-mode swiper wgrep-ag ag hydra visual-regexp anzu paradox counsel ivy-hydra ivy spaceline which-key exec-path-from-shell validate solarized-theme use-package)))
=======
 '(diff-switches "-u -a")
 '(exec-path
   (quote
    ("c:/Python27/Lib/site-packages/PyQt4" "c:/emacs/bin" "C:/Windows/system32" "C:/Windows" "C:/Windows/System32/Wbem" "C:/Windows/System32/WindowsPowerShell/v1.0/" "C:/Program Files/Git/cmd" "C:/Program Files (x86)/Aspell/bin" "C:/Program Files (x86)/Graphviz2.38/bin" "C:/Program Files (x86)/Microsoft SQL Server/Client SDK/ODBC/110/Tools/Binn/" "C:/Program Files (x86)/Microsoft SQL Server/120/Tools/Binn/" "C:/Program Files (x86)/Microsoft SQL Server/120/DTS/Binn/" "C:/Program Files (x86)/Microsoft SQL Server/120/Tools/Binn/ManagementStudio/" "c:/python27" "c:/python27/scripts" "c:/emacs/libexec/emacs/25.1/x86_64-w64-mingw32")))
 '(explicit-shell-file-name "c:/Windows/System32/cmd.exe")
 '(frame-background-mode (quote light))
 '(fringe-mode (quote (16 . 0)) nil (fringe))
 '(magit-git-debug t)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org-home/birthday.org" "~/Dropbox/org-home/newgtd.org")))
 '(package-selected-packages
   (quote
    (pdbtrack org-mode holiday org-capture org-bullets material-theme gitattributes-mode gitignore-mode gitconfig-mode magit rainbow-delimiters ibuffer-projectile virtualenvwrapper ace-link avy ivy-pages visual-fill-column adaptive-wrap whitespace-cleanup-mode easy-kill counsel-projectile projectile js2-mode web-mode pip-requirements company-anaconda anaconda-mode graphviz-dot-mode json-mode markdown-mode flycheck-title flycheck-pos-tip flycheck auto-dictionary company-emoji company-math company-statistics company-quickhelp company yasnippet rainbow-mode highlight-numbers highlight-symbol hl-todo expand-region undo-tree smartparens nlinum reveal-in-osx-finder sudo-edit launch hardhat ignoramus neotree golden-ratio ace-window ibuffer-vc focus-autosave-mode stripe-buffer beacon page-break-lines yaml-mode swiper wgrep-ag ag hydra visual-regexp anzu paradox counsel ivy-hydra ivy spaceline which-key exec-path-from-shell validate solarized-theme use-package)))
>>>>>>> Stashed changes
 '(paradox-github-token t)
 '(pos-tip-background-color nil)
 '(pos-tip-foreground-color "gray")
 '(py-autopep8-options (quote ("--aggressive" "--max-line-length=100")))
 '(racer-rust-src-path nil)
 '(ring-bell-function (lambda nil))
 '(rust-format-on-save t)
 '(safe-local-variable-values
   (quote
    ((flycheck-python-mypy-args "--python-version=3.6" "--strict" "--ignore-missing-imports" "--follow-imports=silent" ".")
     (flycheck-python-mypy-args)
     (flycheck-python-mypy-executable . "/home/dimitern/work/upwork/coastal-advising-py3/run_mypy")
     (flycheck-python-mypy-executable . "/home/dimitern/work/upwork/coastal-advising/run_mypy")
     (projectile-project-run-cmd . "python main.py")
     (pyvenv-workon . "cic-fc"))))
 '(sentence-end-double-space nil)
 '(show-smartparens-global-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil)
 '(w32-fixed-font-alist
   (quote
    ("Font menu"
     ("Input Mono"
      ("Input Mono Light 14pt" "-*-Input Mono Light-normal-r-*-*-14-*-*-*-c-*-*"))
     ("Misc"
      ("fixed" "Fixedsys")
      ("")
      ("Terminal 5x4" "-*-Terminal-normal-r-*-*-*-45-*-*-c-40-*-oem")
      ("Terminal 6x8" "-*-Terminal-normal-r-*-*-*-60-*-*-c-80-*-oem")
      ("Terminal 9x5" "-*-Terminal-normal-r-*-*-*-90-*-*-c-50-*-oem")
      ("Terminal 9x7" "-*-Terminal-normal-r-*-*-*-90-*-*-c-70-*-oem")
      ("Terminal 9x8" "-*-Terminal-normal-r-*-*-*-90-*-*-c-80-*-oem")
      ("Terminal 12x12" "-*-Terminal-normal-r-*-*-*-120-*-*-c-120-*-oem")
      ("Terminal 14x10" "-*-Terminal-normal-r-*-*-*-135-*-*-c-100-*-oem")
      ("Terminal 6x6 Bold" "-*-Terminal-bold-r-*-*-*-60-*-*-c-60-*-oem")
      ("")
      ("Lucida Sans Typewriter.8" "-*-Lucida Sans Typewriter-normal-r-*-*-11-*-*-*-c-*-iso8859-1")
      ("Lucida Sans Typewriter.9" "-*-Lucida Sans Typewriter-normal-r-*-*-12-*-*-*-c-*-iso8859-1")
      ("Lucida Sans Typewriter.10" "-*-Lucida Sans Typewriter-normal-r-*-*-13-*-*-*-c-*-iso8859-1")
      ("Lucida Sans Typewriter.11" "-*-Lucida Sans Typewriter-normal-r-*-*-15-*-*-*-c-*-iso8859-1")
      ("Lucida Sans Typewriter.12" "-*-Lucida Sans Typewriter-normal-r-*-*-16-*-*-*-c-*-iso8859-1")
      ("Lucida Sans Typewriter.8 Bold" "-*-Lucida Sans Typewriter-semibold-r-*-*-11-*-*-*-c-*-iso8859-1")
      ("Lucida Sans Typewriter.9 Bold" "-*-Lucida Sans Typewriter-semibold-r-*-*-12-*-*-*-c-*-iso8859-1")
      ("Lucida Sans Typewriter.10 Bold" "-*-Lucida Sans Typewriter-semibold-r-*-*-13-*-*-*-c-*-iso8859-1")
      ("Lucida Sans Typewriter.11 Bold" "-*-Lucida Sans Typewriter-semibold-r-*-*-15-*-*-*-c-*-iso8859-1")
      ("Lucida Sans Typewriter.12 Bold" "-*-Lucida Sans Typewriter-semibold-r-*-*-16-*-*-*-c-*-iso8859-1"))
     ("Courier"
      ("Courier 10x8" "-*-Courier New-normal-r-*-*-*-97-*-*-c-80-iso8859-1")
      ("Courier 12x9" "-*-Courier New-normal-r-*-*-*-120-*-*-c-90-iso8859-1")
      ("Courier 15x12" "-*-Courier New-normal-r-*-*-*-150-*-*-c-120-iso8859-1")
      ("")
      ("8" "-*-Courier New-normal-r-*-*-11-*-*-*-c-*-iso8859-1")
      ("9" "-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-1")
      ("10" "-*-Courier New-normal-r-*-*-13-*-*-*-c-*-iso8859-1")
      ("11" "-*-Courier New-normal-r-*-*-15-*-*-*-c-*-iso8859-1")
      ("12" "-*-Courier New-normal-r-*-*-16-*-*-*-c-*-iso8859-1")
      ("8 bold" "-*-Courier New-bold-r-*-*-11-*-*-*-c-*-iso8859-1")
      ("9 bold" "-*-Courier New-bold-r-*-*-12-*-*-*-c-*-iso8859-1")
      ("10 bold" "-*-Courier New-bold-r-*-*-13-*-*-*-c-*-iso8859-1")
      ("11 bold" "-*-Courier New-bold-r-*-*-15-*-*-*-c-*-iso8859-1")
      ("12 bold" "-*-Courier New-bold-r-*-*-16-*-*-*-c-*-iso8859-1")
      ("8 italic" "-*-Courier New-normal-i-*-*-11-*-*-*-c-*-iso8859-1")
      ("9 italic" "-*-Courier New-normal-i-*-*-12-*-*-*-c-*-iso8859-1")
      ("10 italic" "-*-Courier New-normal-i-*-*-13-*-*-*-c-*-iso8859-1")
      ("11 italic" "-*-Courier New-normal-i-*-*-15-*-*-*-c-*-iso8859-1")
      ("12 italic" "-*-Courier New-normal-i-*-*-16-*-*-*-c-*-iso8859-1")
      ("8 bold italic" "-*-Courier New-bold-i-*-*-11-*-*-*-c-*-iso8859-1")
      ("9 bold italic" "-*-Courier New-bold-i-*-*-12-*-*-*-c-*-iso8859-1")
      ("10 bold italic" "-*-Courier New-bold-i-*-*-13-*-*-*-c-*-iso8859-1")
      ("11 bold italic" "-*-Courier New-bold-i-*-*-15-*-*-*-c-*-iso8859-1")
      ("12 bold italic" "-*-Courier New-bold-i-*-*-16-*-*-*-c-*-iso8859-1")))))
 '(when (eq system-type (quote windows-nt)) t)
 '(x-gtk-use-system-tooltips nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#F0F0F0" :inverse-video nil))))
 '(company-tooltip-selection ((t (:background "DeepSkyBlue4" :foreground "#ffffff" :weight bold))))
 '(neo-banner-face ((t (:foreground "magenta"))))
 '(neo-dir-link-face ((t (:foreground "green"))))
 '(neo-expand-btn-face ((t (:foreground "cyan"))))
 '(neo-file-link-face ((t (:foreground "deep sky blue"))))
 '(neo-header-face ((t (:foreground "magenta"))))
 '(neo-root-dir-face ((t (:foreground "magenta"))))
 '(nlinum-current-line ((t (:inherit linum :background "dim gray" :foreground "white" :weight bold))))
 '(tooltip ((t (:background "dim gray" :foreground "black" :box (:line-width 2 :color "grey75" :style released-button) :weight normal)))))
