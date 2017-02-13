;; dimitern-frames.el: Global window system, and frames config.
;;

;;;###autoload
(defun dimitern-frames/no-bars ()
  "Disable both the tool bar and scroll bar.
Menu bar is also disabled unless on darwin GUI."
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (unless (and
           (dimitern-os/is-darwin)
           (display-graphic-p))
    (menu-bar-mode -1)))

;; face-remap: face remapping (text scaling)
(use-package face-remap
  :bind (("C-c w z" . text-scale-adjust)))

;; frame: global frames config.
(use-package frame
  :bind (("C-c w F" . toggle-frame-fullscreen))
  :config
  ;; Kill C-z for `suspend-frame', keep C-x C-z only.
  (global-set-key (kbd "C-z") nil)
  (global-set-key (kbd "C-x C-z") 'suspend-frame)
  ;; Allow toggling the menu bar with F1.
  (global-set-key [f1] 'menu-bar-mode)
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (validate-setq
   ;; No audible bell (ding), use visual bell instead.
   visible-bell t
   ;; Resize frames by pixels.
   frame-resize-pixelwise t
   ;; More useful frame names.
   frame-title-format '(:eval (if (buffer-file-name)
                                  (abbreviate-file-name (buffer-file-name)) "%b"))
   ;; Size new windows proportionally wrt other windows
   window-combination-resize t
   )
  )

(provide 'dimitern-frames)
