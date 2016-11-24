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

(defvar dimitern-theme/theme nil
  "Selected default theme depending on system-type.")

(defun dimitern-theme/load-default () 
  "Load the default theme, depending on system-type."
  (if (dimitern-os/is-darwin)
      ;; solarized-light for darwih, with bigger font size.
      (progn
	(load-theme 'solarized-light 'no-confirm 'no-enable)
	(customize-set-variable 'frame-background-mode 'light)
	(setq dimitern-theme/theme 'solarized-light)
	(set-face-attribute 'default nil :height (* 10 (frame-char-height))))
    ;; solarized-dark for linux.
    (load-theme 'solarized-dark 'no-confirm 'no-enable)
    (validate-setq
     dimitern-theme/theme 'solarized-dark
     ;; Needed for fonts installation to work.
     font-use-system-font t
     )
    ;; Install font if needed (linux only).
    (if (member dimitern-theme/font-family (font-family-list))
	t
      (when (display-graphic-p)
	;; Font installation only works on Linux.
	(dimitern-theme/install-font)
	(add-to-list 'initial-frame-alist (cons 'font (dimitern-theme/font)))
	(add-to-list 'default-frame-alist (cons 'font (dimitern-theme/font)))))))

;; Solarized theme config.
(use-package solarized-theme
  :ensure t
  :after frame
  :config  
  (dimitern-theme/load-default)
  (validate-setq
   ;; Disable variable pitch fonts in Solarized theme
   solarized-use-variable-pitch nil
   ;; Prefer italics over bold.
   solarized-use-less-bold t
   solarized-use-more-italic t
   ;; Emphasize docstrings.
   solarized-distinct-doc-face t
   )
  (enable-theme dimitern-theme/theme))

(provide 'dimitern-theme)
