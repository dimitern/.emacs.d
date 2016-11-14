;; ----------------------------------------------------------------------------
;; init: Master
;; ----------------------------------------------------------------------------


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun dimitern/add-to-load-path (path)
  (add-to-list 'load-path (expand-file-name path user-emacs-directory)))

(dimitern/add-to-load-path "lisp")

(require 'dimitern-el-get)
(require 'dimitern-use-package)

(use-package dimitern-customize
  :load-path "lisp/")
