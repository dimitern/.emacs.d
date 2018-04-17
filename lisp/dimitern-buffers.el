;; dimitern-buffers.el: global buffers-related config.
;;

;; page-break-lines: turn page breaks (^L) into lines.
(use-package page-break-lines
  :ensure t
  :defer 1
  :config
  (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

;; multiple-cursors: add many cursors
(use-package multiple-cursors
  :ensure t
  :defer 1
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)))

;; beacon: highlight cursor position in buffer (when switching).
(use-package beacon
  :ensure t
  :defer 1
  :config
  (validate-setq
   beacon-blink-duration 0.05
   beacon-blink-delay 0.1)
  (beacon-mode 1)
  :diminish beacon-mode)

;; hl-line: highlight the current line in buffer.
(use-package hl-line
  :defer 1
  :config
  (global-hl-line-mode 1))

;; stripe-buffer: add stripes to buffers (lists).
(use-package stripe-buffer
  :ensure t
  :defer 1
  :config
  (add-hook 'dired-mode-hook #'stripe-buffer-mode))

;; focus-autosave-mode: save buffers when focus is lost.
(use-package focus-autosave-mode
  :ensure t
  :defer 1
  :config
  (focus-autosave-mode)
  :diminish focus-autosave-mode)

(use-package minibuffer
  :bind (("C-c M-b k" . dimitern-buffers/kill-this)
         ("C-c M-b m" . gpc/mirror-image))

  :config
  (add-hook 'kill-buffer-query-functions
            #'dimitern-buffers/do-not-kill-important)

  ;; Don't kill the important buffers
  (defconst dimitern-buffers/do-not-kill-names '("*scratch*" "*Messages*")
    "Names of buffers that should not be killed.")

  ;; <source: https://emacs.stackexchange.com/questions/7198/indirect-buffer-in-image-mode-main-buffer-in-text>
  (defun gpc/mirror-buffer (buffer-name &optional more-after-change)
    "Create a buffer whose contents will follow the current one's
and returns the new buffer.  Runs `more-after-change' after each
change if provided.

This differs from `clone-indirect-buffer' in that the new buffer
is not visiting a file.  It's really just a kludge to support
`gpc/mirror-image', which see."
    (interactive (list
                  (let ((default (concat (buffer-name) "<mirror>")))
                    (read-string "Buffer name: " default
                                 nil nil default))))
    (make-local-variable 'after-change-functions)
    (make-local-variable 'kill-buffer-hook)
    (lexical-let*
        ((target-buffer (generate-new-buffer buffer-name))
         ;; Give lexical scope to arg
         (after-change more-after-change)
         (copy-change
          #'(lambda(start end old-len)
              (let ((inhibit-read-only t))
                ;; Quick and dirty: may not be suitable for large buffers.
                (copy-to-buffer target-buffer (point-min) (point-max))
                (when (functionp after-change)
                  (funcall after-change target-buffer))))))

      ;; Initialize the target buffer with the source text.
      (copy-to-buffer target-buffer (point-min) (point-max))

      (add-hook 'after-change-functions copy-change t t)

      ;; Cleanup hooks.

      ;; Kill the other buffer if the source buffer is closed.
      (add-hook 'kill-buffer-hook
                #'(lambda () (kill-buffer target-buffer)) t t)

      ;; Destroy the change hook if the other buffer is killed.
      (with-current-buffer target-buffer
        (make-local-variable 'kill-buffer-hook)
        (add-hook 'kill-buffer-hook
                  #'(lambda ()
                      (remove-hook 'after-change-functions copy-change t))
                  t t))))

  (defun gpc/mirror-image ()
    "Open an `image-mode' buffer that tracks the content of the
current buffer.  Intended for use with svg files."
    (interactive)
    (image-mode-as-text)
    (let* ((buffer-name (concat (buffer-name) "<image>"))
           ;; An `image-mode' buffer will switch back to text when its contents
           ;; are replaced.  Besides, the image is not updated in-place when the
           ;; content changes, so you'd have to toggle back to image-mode anyway.
           (after-change '(lambda (buffer)
                            (with-current-buffer buffer (image-mode))))
           (mirror (gpc/mirror-buffer buffer-name after-change)))
      (split-window)
      (other-window 1)
      (switch-to-buffer buffer-name)
      (image-mode)
      (other-window 1)))
  ;; </source>

  (defun dimitern-buffers/do-not-kill-important ()
    "Inhibit killing of important buffers.
Add this to `kill-buffer-query-functions'."
    (if (not (member (buffer-name) dimitern-buffers/do-not-kill-names))
        t
      (message "Not allowed to kill %s, burying instead" (buffer-name))
      (bury-buffer)
      nil))

  (defun dimitern-buffers/kill-this ()
    "Kill the current buffer."
    (interactive)
    (kill-buffer (current-buffer)))

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
     ("." nil (reusable-frames . visible)))))

;; uniquify: make buffer names unique.
(use-package uniquify
  :defer 1
  :config
  (validate-setq
   uniquify-buffer-name-style 'forward))

;; ibuffer: better buffer list.
(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer))
  ;; Show VC Status in ibuffer
  :config
  (validate-setq
   ibuffer-formats
   '((mark modified read-only vc-status-mini " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           (vc-status 16 16 :left)
           " "
           filename-and-process)
     (mark modified read-only " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " " (name 16 -1) " " filename))))

;; ibuffer-vc: group buffers by VC project and status.
(use-package ibuffer-vc
  :ensure t
  :after ibuffer
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;; ibuffer-projectile: group buffers by Projectile project.
(use-package ibuffer-projectile
  :ensure t
  :after projectile
  :defer t
  :init (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))

;; easy-kill: easy killing and marking on C-w.
(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp]      . easy-mark)))

;; Make Tab complete if the line is indented
(validate-setq tab-always-indent 'complete)

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(validate-setq indicate-empty-lines t
               require-final-newline t
               enable-recursive-minibuffers t)

;; Configure a reasonable fill column, indicate it in the buffer and enable
;; automatic filling
(setq-default fill-column 100)
(add-hook 'text-mode-hook #'auto-fill-mode)
(diminish 'auto-fill-function " Ⓕ")

(bind-key "C-c x i" #'indent-region)
(bind-key "C-c x d" #'downcase-dwim)
(bind-key "C-c x u" #'upcase-dwim)

;; delsel: delete the selection instead of insert
(use-package delsel
  :defer t
  :init (delete-selection-mode))

;; newcomment: built-in comment features.
(use-package newcomment
  :bind (("C-c M-c d" . comment-dwim)
         ("C-c M-c l" . comment-line)
         ("C-c M-c r" . comment-region)))

;; whitespace-cleanup-mode: cleanup whitespace in buffers.
(use-package whitespace-cleanup-mode
  :ensure t
  :bind (("C-c t c" . whitespace-cleanup-mode)
         ("C-c x w" . whitespace-cleanup))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :diminish (whitespace-cleanup-mode . "🅦"))

;; subword: subword/superword editing.
(use-package subword
  :defer t
  :diminish subword-mode)

;; adaptive-wrap: choose wrap prefix automatically.
(use-package adaptive-wrap
  :ensure t
  :pin "gnu"
  :defer t
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

;; visual-fill-column: fill column wrapping for Visual Line Mode.
(use-package visual-fill-column
  :ensure t
  :defer t
  :bind (("C-c t v" . visual-fill-column-mode))
  :init
  ;; Turn on whenever visual line mode is on, and in all text or prog mode
  ;; buffers to get centered text
  (dolist (hook '(visual-line-mode-hook
                  prog-mode-hook
                  text-mode-hook))
    (add-hook hook #'visual-fill-column-mode))
  (add-hook 'python-mode-hook #'(lambda () (visual-fill-column-mode -1)))
  ;; Center text by default, and move the fringes close to the text.
  :config
  (setq-default visual-fill-column-center-text nil
                visual-fill-column-fringes-outside-margins nil)
  ;; Split windows vertically despite large margins, because Emacs otherwise
  ;; refuses to vertically split windows with large margins
  (validate-setq split-window-preferred-function
                 #'visual-fill-column-split-window-sensibly))

(provide 'dimitern-buffers)
