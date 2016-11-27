;; dimitern-custom.el: Easy Customization config.
;;

(defconst dimitern/custom-file (locate-user-emacs-file "custom")
  "File used to store settings from Easy Customization UI.")

(use-package cus-edit
  :defer t
  :config
  (validate-setq
   custom-file (concat dimitern/custom-file ".el")
   ;; Kill buffer when existing.
   custom-buffer-done-kill t
   ;; Remove redundant help text.
   custom-buffer-verbose-help nil
   ;; Show the real variable names.
   custom-unlispify-tag-names nil
   custom-unlispify-menu-entries nil
   )
  :init
  (load dimitern/custom-file 'no-error)
  )

(provide 'dimitern-custom)
