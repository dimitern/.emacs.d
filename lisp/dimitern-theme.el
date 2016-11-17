;; dimitern-theme.el: Emacs theme and fonts setup.
;;

;; Default font (FIXME: only works on Ubuntu).
(defvar dimitern/default-font-family "Input Mono")
(defvar dimitern/default-font-name "Light")
(defvar dimitern/default-font-size 9)
(defvar dimitern/default-font-dir "fonts/Input-Font/")

(defun dimitern/default-font ()
  "Default font family, name, and size to use."
  (format "%s %s-%s"
	  dimitern/default-font-family
	  dimitern/default-font-name
	  dimitern/default-font-size))

(defun dimitern/install-default-font ()
    (let ((home-font-dir (format "~/.%s" dimitern/default-font-dir))
	  (emacs-font-dir (format "~/.emacs.d/%s" dimitern/default-font-dir)))
      (when (not (member dimitern/default-font-family (font-family-list)))
	(progn
	  (message (format "dimitern: Installing fonts in '%s'" dimitern/default-font-dir))
	  (shell-command (format "cp -R %s %s" emacs-font-dir home-font-dir))
	  (message "dimitern: Rebuilding fonts cache...")
	  (shell-command (format "fc-cache -r %s" home-font-dir))))))

;; Install and enable solarized dark theme, installling font if needed.
(setq color-themes '())
(setq font-use-system-font t) ;; needed for fonts installation to work.

(use-package solarized-theme
  :if (display-graphic-p)
  :ensure t
  :init
  (progn
    (unless (member dimitern/default-font-family (font-family-list))
      (dimitern/install-default-font))
    (add-to-list 'initial-frame-alist (cons 'font (dimitern/default-font)))
    (add-to-list 'default-frame-alist (cons 'font (dimitern/default-font)))
    (customize-set-variable 'frame-background-mode 'dark)
    (setq solarized-termcolors 256)
    (load-theme 'solarized-dark 'no-confirm 'no-enable))
    :config
    (enable-theme 'solarized-dark))

(provide 'dimitern-theme)
