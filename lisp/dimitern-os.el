;; dimitern-os.el: OS-specific helper functions.
;;

(defun dimitern-os/is-linux ()
  "Returns non-nil if running on Linux OS."
  (eq system-type 'gnu/linux))

(defun dimitern-os/is-darwin ()
  "Returns non-nil if running on Mac OS X (darwin)."
  (eq system-type 'darwin))

(defun dimitern-os/is-windows ()
  "Returns non-nil if running on Windows."
  (eq system-type 'windows-nt))

(provide 'dimitern-os)
