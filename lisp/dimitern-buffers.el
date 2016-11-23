;; dimitern-buffers.el: global buffers-related config.
;;

;; Configure `display-buffer' behaviour for some special buffers.
(validate-setq
 display-buffer-alist
 `(
   ;; Put REPLs and error lists into the bottom side window
   (,(rx bos
         (or "*Help"                         ; Help buffers
             "*Warnings*"                    ; Emacs warnings
             "*Compile-Log*"                 ; Emacs byte compiler log
             "*compilation"                  ; Compilation buffers
             "*Flycheck errors*"             ; Flycheck error list
             "*shell"                        ; Shell window
             "*SQL"                          ; SQL REPL
             (and (1+ nonl) " output*")      ; AUCTeX command output
             ))
    (display-buffer-reuse-window
     display-buffer-in-side-window)
    (side            . bottom)
    (reusable-frames . visible)
    (window-height   . 0.33))
   ;; Let `display-buffer' reuse visible frames for all buffers.  This must
   ;; be the last entry in `display-buffer-alist', because it overrides any
   ;; later entry with more specific actions.
   ("." nil (reusable-frames . visible))))

;; page-break-lines: turn page breaks (^L) into lines.
(use-package page-break-lines
  :ensure t
  :init
  (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

;; beacon: highlight cursor position in buffer (when switching).
(use-package beacon
  :ensure t
  :init
  (beacon-mode 1)
  :diminish beacon-mode)

;; hl-line: highlight the current line in buffer.
(use-package hl-line
  :init
  (global-hl-line-mode 1))

;; stripe-buffer: add stripes to buffers (lists).
(use-package stripe-buffer
  :ensure t
  :init
  (add-hook 'dired-mode-hook #'stripe-buffer-mode))

;; focus-autosave-mode: save buffers when focus is lost.
(use-package focus-autosave-mode
  :ensure t
  :init
  (focus-autosave-mode)
  :diminish focus-autosave-mode)

;; Don't kill the important buffers
(defconst dimitern-buffers/do-not-kill-names '("*scratch*" "*Messages*")
  "Names of buffers that should not be killed.")

(defun dimitern-buffers/do-not-kill-important ()
  "Inhibit killing of important buffers.
Add this to `kill-buffer-query-functions'."
  (if (not (member (buffer-name) dimitern-buffers/do-not-kill-names))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))

(use-package minibuffer
  :bind (("C-c b k" . dimitern-buffers/kill-this)) 
  :config
  (add-hook 'kill-buffer-query-functions
            #'dimitern-buffers/do-not-kill-important))

;;;###autoload
(defun dimitern-buffers/kill-this ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
 
(provide 'dimitern-buffers)
