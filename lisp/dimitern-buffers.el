;; dimitern-buffers.el: global buffers-related config.
;;

;; page-break-lines: turn page breaks (^L) into lines.
(use-package page-break-lines
  :ensure t
  :defer 1
  :config
  (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

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
  :bind (("C-c b k" . dimitern-buffers/kill-this))
  :config
  (add-hook 'kill-buffer-query-functions
            #'dimitern-buffers/do-not-kill-important)

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
(setq-default fill-column 80)
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
  :bind (("C-c c d" . comment-dwim)
         ("C-c c l" . comment-line)
         ("C-c c r" . comment-region)))

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
