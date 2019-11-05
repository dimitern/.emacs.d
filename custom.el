(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-highlight-search t)
 '(anaconda-mode-eldoc-as-single-line nil)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238"))
 '(blacken-line-length (quote fill))
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
 '(company-idle-delay 0.1)
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
 '(custom-enabled-themes (quote (doom-tomorrow-day)))
 '(custom-safe-themes
   (quote
    ("fd944f09d4d0c4d4a3c82bd7b3360f17e3ada8adf29f28199d09308ba01cc092" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "9954ed41d89d2dcf601c8e7499b6bb2778180bfcaeb7cdfc648078b8e05348c6" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "a8c210aa94c4eae642a34aaf1c5c0552855dfca2153fa6dd23f3031ce19453d4" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "7e559cd74b715c744ff8cf276a774e4a087ae7065062cb258bc3ef30faecb55b" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "c1ab17435aaac39206b0f34dddda6349d91004c27258ab55301b973b179ddd55" "b63c0298290e6c6aa17d64fbce2c2a83c4b420d73c378f4f96dd8e1883ac6d51" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" default)))
 '(doom-modeline-mode t)
 '(ein:default-url-or-port "http://127.0.0.1:8889")
 '(ein:enable-keepalive t)
 '(ein:url-or-port (quote ("http://127.0.0.1:8889")))
 '(enable-local-variables :safe)
 '(fci-rule-color "#37474f")

 '(flycheck-disabled-checkers (quote (python-pylint python-pycompile)))

 '(flycheck-checkers
   (quote
    (ada-gnat asciidoctor asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint cwl d-dmd dockerfile-hadolint elixir-dogma emacs-lisp emacs-lisp-checkdoc erlang-rebar3 erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-megacheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-standard json-jsonlint json-python-json jsonnet less less-stylelint llvm-llc lua-luacheck lua perl perl-perlcritic php php-phpmd php-phpcs processing proselint protobuf-protoc pug puppet-parser puppet-lint r-lintr racket rpm-rpmlint markdown-mdl nix rst-sphinx rst ruby-rubocop ruby-reek ruby-rubylint ruby ruby-jruby rust-cargo rust scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tcl-nagelfar tex-chktex tex-lacheck texinfo typescript-tslint verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby python-mypy python-flake8)))
 '(flycheck-disabled-checkers (quote (python-pycompile python-pylint python-mypy)))
 '(flycheck-display-errors-function nil)
 '(flycheck-executable-find (quote flycheck-default-executable-find))
 '(flycheck-flake8-maximum-complexity 10)
 '(flycheck-flake8-maximum-line-length 120)
 '(flycheck-flake8rc ".flake8")
 '(flycheck-go-build-executable nil)
 '(flycheck-go-gofmt-executable "gofmt")
 '(flycheck-go-golint-executable "golint")
 '(flycheck-go-vet-executable "govet")
 '(flycheck-pycheckers-checkers (quote (flake8 pyflakes)))
 '(flycheck-pycheckers-max-line-length 120)
 '(flycheck-pycheckers-report-errors-inline "false")
 '(flycheck-pycheckers-venv-root "~/.local/share/virtualenvs")
 '(flycheck-python-flake8-executable "flake8")
 '(flycheck-python-mypy-args
   (quote
    ("--follow-imports=silent" "--ignore-missing-imports")))
 '(flycheck-python-mypy-executable "mypy")
 '(flycheck-python-pylint-executable
   "/home/dimitern/work/upwork/metronom-pro-backend/.venv/bin/pylint")
 '(flycheck-syntax-check-failed-hook (quote (ignore)))
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
  '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#fd971f"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#b6e63e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#525254"))
 '(js-indent-level 2)
 '(json-reformat:indent-width 2)
 '(linum-delay t)
 '(linum-eager nil)
 '(magit-git-debug t t)
 '(markdown-asymmetric-header t)
 '(markdown-css-paths
   (quote
    ("https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.css")))
 '(markdown-enable-html nil)
 '(mouse-wheel-scroll-amount (quote (0.07)))
 '(neo-autorefresh nil)
 '(neo-hidden-regexp-list
   (quote
    ("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "__pycache__")))
 '(neo-show-hidden-files nil)
 '(neo-theme (quote nerd))
 '(neo-vc-integration (quote (face char)))
 '(nlinum-highlight-current-line t)
<'(nlinum-use-right-margin t)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org-home/newgtd.org" "~/Dropbox/org-home/birthday.org")))
 '(package-selected-packages
   (quote
    (haml-mode pycoverage ov py-isort nginx-mode nginx company-racer company-rust flycheck-rust racer rust-mode rust flycheck-mypy material-light material ein go-errcheck errcheck go-mode pydoc multiple-cursors package-lint py-autopep8 org-mobile org-mode holiday org-capture org-bullets material-theme gitattributes-mode gitignore-mode gitconfig-mode magit rainbow-delimiters ibuffer-projectile virtualenvwrapper ace-link avy ivy-pages visual-fill-column adaptive-wrap whitespace-cleanup-mode easy-kill counsel-projectile projectile js2-mode web-mode pip-requirements company-anaconda anaconda-mode graphviz-dot-mode json-mode markdown-mode flycheck-title flycheck-pos-tip flycheck auto-dictionary company-emoji company-math company-statistics company-quickhelp company yasnippet rainbow-mode highlight-numbers highlight-symbol hl-todo expand-region undo-tree smartparens nlinum reveal-in-osx-finder sudo-edit launch hardhat ignoramus neotree golden-ratio ace-window ibuffer-vc focus-autosave-mode stripe-buffer beacon page-break-lines yaml-mode swiper wgrep-ag ag hydra visual-regexp anzu paradox counsel ivy-hydra ivy spaceline which-key exec-path-from-shell validate solarized-theme use-package)))
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
 '(nlinum-use-right-margin nil)
 '(notmuch-search-oldest-first nil)
 '(notmuch-show-relative-dates nil)
 '(objed-cursor-color "#e74c3c")
 '(org-agenda-files
   (quote
    ("~/Nextcloud/Dropbox/org-home/newgtd.org" "~/Nextcloud/Dropbox/org-home/birthday.org")))
 '(org-export-backends (quote (ascii html icalendar md odt)))
 '(org-fontify-quote-and-verse-blocks t)
 '(org-hide-block-startup t)
 '(org-list-indent-offset 2)
 '(org-pretty-entities t)
 '(package-selected-packages
   (quote
    (treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs doom-modeline doom-themes all-the-icons dockerfile-mode dockerfile flycheck-flake8 blacken blacken-mode pipenv inf-clojure clojure clojure-mode htmlize org-edna org-reveal ox-reveal haml-mode ov nginx-mode nginx company-racer company-rust flycheck-rust rust flycheck-mypy material-light material ein go-errcheck errcheck go-mode multiple-cursors package-lint org-mobile org-mode holiday org-capture org-bullets material-theme gitattributes-mode gitignore-mode gitconfig-mode magit ibuffer-projectile ace-link ivy-pages counsel-projectile pip-requirements company-anaconda graphviz-dot-mode markdown-mode flycheck-title flycheck-pos-tip flycheck auto-dictionary company-emoji company-statistics company-quickhelp rainbow-mode highlight-numbers highlight-symbol hl-todo expand-region undo-tree nlinum launch hardhat ignoramus neotree golden-ratio ibuffer-vc focus-autosave-mode page-break-lines ag anzu paradox counsel ivy-hydra exec-path-from-shell validate solarized-theme)))
 '(paradox-execute-asynchronously t)
 '(paradox-github-token t)
 '(paradox-hide-wiki-packages nil)
 '(paradox-use-homepage-buttons t)
 '(pos-tip-background-color nil)
 '(pos-tip-foreground-color "gray")
 '(projectile-after-switch-project-hook (quote (ignore)))
 '(projectile-sort-order (quote recentf))
 '(projectile-switch-project-action (quote projectile-recentf))
 '(py-autopep8-options nil)
 '(py-isort-options
   (quote
    ("--multi-line=3" "--trailing-comma" "--force-grid-wrap=0" "--use-parentheses" "--line-width=120")))
 '(python-shell-interpreter "ipython")
>>>>>>> Last changes.
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
     (py-isort-options . t)
     (nlinum-mode . t)
     (flycheck-python-flake8-executable . flake8)
     (py-isort-mode . t)
     (blacken-mode . t)
     (pythonic-activate . \.venv)
     (flycheck-checker . python-flake8)
))))
 '(sentence-end-double-space nil)
 '(show-smartparens-global-mode t)
 '(standard-indent 2)
 '(tab-width 4)
 '(term-input-ignoredups t)
 '(tool-bar-mode nil)
 '(treemacs-show-hidden-files nil)
 '(treemacs-width 50)
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
 '(visual-fill-column-width nil)
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
 '(web-mode-code-indent-offset 4)
 '(web-mode-css-indent-offset 4)
 '(web-mode-enable-element-tag-fontification t)
 '(when (eq system-type (quote windows-nt)) t)
 '(whitespace-line-column 88)
 '(x-gtk-use-system-tooltips nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#F0F0F0" :inverse-video nil))))
 '(company-tooltip-selection ((t (:background "DeepSkyBlue4" :foreground "#ffffff" :weight bold))))
 '(highlight-symbol-face ((t (:background "light steel blue"))))
 '(neo-banner-face ((t (:foreground "magenta"))))
 '(neo-dir-link-face ((t (:foreground "green"))))
 '(neo-expand-btn-face ((t (:foreground "cyan"))))
 '(neo-file-link-face ((t (:foreground "deep sky blue"))))
 '(neo-header-face ((t (:foreground "magenta"))))
 '(neo-root-dir-face ((t (:foreground "magenta"))))
 '(nlinum-current-line ((t (:inherit linum :background "dim gray" :foreground "white" :weight bold))))
