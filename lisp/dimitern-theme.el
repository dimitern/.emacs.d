;; ----------------------------------------------------------------------------
;; dimitern-theme: Setup color-theme and fonts.
;; ----------------------------------------------------------------------------

;; Color theme setup.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized-master/")
(load-theme 'solarized t)

;; Default font.
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

;; Without this installation below will fail without restarting Emacs.
(setq font-use-system-font t)

(defun dimitern/install-default-font ()
    (let ((home-font-dir (format "~/.%s" dimitern/default-font-dir))
	  (emacs-font-dir (format "~/.emacs.d/%s" dimitern/default-font-dir)))
      (when (not (member dimitern/default-font-family (font-family-list)))
	(progn
	  (message (format "dimitern: Installing fonts in '%s'" dimitern/default-font-dir))
	  (shell-command (format "cp -R %s %s" emacs-font-dir home-font-dir))
	  (message "dimitern: Rebuilding fonts cache...")
	  (shell-command (format "fc-cache -r %s" home-font-dir))))))

(unless (member dimitern/default-font-family (font-family-list))
     (dimitern/install-default-font))

(add-to-list 'initial-frame-alist (cons 'font (dimitern/default-font)))
(add-to-list 'default-frame-alist (cons 'font (dimitern/default-font))) 
(set-frame-font (dimitern/default-font) nil t)
(set-face-attribute 'default nil :font (dimitern/default-font))

(provide 'dimitern-theme)
