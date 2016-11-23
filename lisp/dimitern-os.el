;; dimitern-os.el: OS-specific helper functions.
;;

(defun dimitern-os/is-linux ()
  "Returns non-nil if running on Linux OS."
  (eq system-type 'gnu/linux))

(defun dimitern-os/is-darwin ()
  "Returns non-nil if running on Mac OS X (darwin)."
  (eq system-type 'darwin))

(provide 'dimitern-os)
