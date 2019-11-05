;; dimitern-theme.el: Emacs theme and fonts setup.
;;

;; Default font (FIXME: only works on Ubuntu).
(defvar dimitern-theme/font-family
  (if (dimitern-os/is-windows)
      "InputMono"
    "Input Mono")
  "Font family for the theme.")
(defvar dimitern-theme/font-name "Light"
  "Font name for the theme.")
(defvar dimitern-theme/font-size
  (if (dimitern-os/is-windows)
      10
    9)
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

(defun dimitern-theme/load-default ()
  "Load the default theme, depending on `system-type'."
  (load-theme 'material t t)
  (if (dimitern-os/is-darwin)
      ;; bigger font size for darwin.
      (set-face-attribute 'default nil :height (* 10 (frame-char-height)))

    (when (dimitern-os/is-darwin)
      (setq
       ;; Needed for fonts installation to work.
       font-use-system-font t))
    ;; Install font if needed (linux only).
    (if (and
         (dimitern-os/is-linux)
         (member dimitern-theme/font-family (font-family-list)))
        t
      (when (display-graphic-p)
        ;; Font installation only works on Linux.
        (unless (dimitern-os/is-windows) (dimitern-theme/install-font))
        (add-to-list 'initial-frame-alist (cons 'font (dimitern-theme/font)))
        (add-to-list 'default-frame-alist (cons 'font (dimitern-theme/font)))))))

(use-package doom-themes
  :ensure t
  :pin "melpa"
  :after frame
  :config
  ;; (load-theme 'doom-tomorrow-day t nil)
  (load-theme 'doom-vibrant t nil)
  )

;; ;; Material theme config.
;; (use-package material-theme
;;   :ensure t
;;   :after frame
;;   :config
;;   ;;(dimitern-theme/load-default)
;;   (enable-theme 'material))

(provide 'dimitern-theme)
