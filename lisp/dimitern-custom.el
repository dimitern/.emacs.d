;; dimitern-custom.el: Easy Customization config.

(defconst dimitern-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Easy Customization UI.")

(use-package cus-edit
  :defer t
  :config
  (validate-setq
   custom-file dimitern-custom-file
   ;; Kill buffer when existing.
   custom-buffer-done-kill t
   ;; Remove redundant help text.
   custom-buffer-verbose-help nil
   ;; Show the real variable names.
   custom-unlispify-tag-names nil
   custom-unlispify-menu-entries nil
   )
  :init
  (load dimitern-custom-file 'no-error 'no-message)
  )

(provide 'dimitern-custom)
