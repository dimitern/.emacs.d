;; ----------------------------------------------------------------------------
;; dimitern-use-package: Setup use-package
;; ----------------------------------------------------------------------------

;; Add standard repositories.
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")
	("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Install if missing.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(provide 'dimitern-use-package)
