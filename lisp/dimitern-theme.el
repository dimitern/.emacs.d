;; dimitern-theme.el: Emacs theme and fonts setup.
;;

;; Default font (FIXME: only works on Ubuntu).
(defvar dimitern-theme/font-family "Input Mono"
  "Font family for the theme.")
(defvar dimitern-theme/font-name "Light"
  "Font name for the theme.")
(defvar dimitern-theme/font-size 9
  "Font size for the theme.")
(defvar dimitern-theme/font-dir "fonts/Input-Font/"
  "Fonts directory prefix.")

(defun dimitern-theme/font ()
  "Default font family, name, and size to use."
  (format "%s %s-%s"
	  dimitern-theme/font-family
	  dimitern-theme/font-name
	  dimitern-theme/font-size))

(defun dimitern-theme/install-font ()
  "Install default font when missing."
    (let ((home-font-dir (format "~/.%s" dimitern-theme/font-dir))
	  (emacs-font-dir (format "~/.emacs.d/%s" dimitern-theme/font-dir)))
      (when (not (member dimitern-theme/font-family (font-family-list)))
	(message (format "dimitern: Installing fonts in '%s'" dimitern-theme/font-dir))
	(shell-command (format "cp -R %s %s" emacs-font-dir home-font-dir))
	(message "dimitern: Rebuilding fonts cache...")
	(shell-command (format "fc-cache -r %s" home-font-dir)))))

;; Install and enable solarized dark theme, installling font if needed.
(use-package solarized-theme
  :if (or
       (display-graphic-p)
       (dimitern-os/is-darwin))
  :ensure t
  :init
  (load-theme 'solarized-dark 'no-confirm 'no-enable)
  (unless (member dimitern-theme/font-family (font-family-list))
    ;; Font installation only works on Linux.
    (unless (dimitern-os/is-darwin)
      (dimitern-theme/install-font)
      (add-to-list 'initial-frame-alist (cons 'font (dimitern-theme/font)))
      (add-to-list 'default-frame-alist (cons 'font (dimitern-theme/font)))))
  :config
  (validate-setq
   ;; Needed for fonts installation to work.
   font-use-system-font t
   ;; Disable variable pitch fonts in Solarized theme
   solarized-use-variable-pitch nil
   ;; Prefer italics over bold.
   solarized-use-less-bold t
   solarized-use-more-italic t
   ;; Emphasize docstrings.
   solarized-distinct-doc-face t
   )
  (customize-set-variable 'frame-background-mode 'dark)
  (enable-theme 'solarized-dark))

(provide 'dimitern-theme)
