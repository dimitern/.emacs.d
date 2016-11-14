;; ----------------------------------------------------------------------------
;; dimitern-el-get: Setup el-get
;; ----------------------------------------------------------------------------

;; Put all downloaded packages here:
(setq el-get-user-package-directory "~/.emacs.d/packages.d/")

;; Load at startup.
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; Install if missing.
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; My recipes go here:
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(el-get 'sync)

(provide 'dimitern-el-get)
