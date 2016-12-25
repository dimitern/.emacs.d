;; dimitern-windows.el: additional window system config and helpers.
;;

(defun dimitern-windows/find-side-windows (&optional side)
  "Get all side window if any.
If SIDE is non-nil only get windows on that side."
  (let (windows)
    (walk-window-tree
     (lambda (window)
       (let ((window-side (window-parameter window 'window-side)))
         (when (and window-side (or (not side) (eq window-side side)))
           (push window windows)))))
    windows))

(defun dimitern-windows/quit-all-side-windows ()
  "Quit all side windows of the current frame."
  (interactive)
  (dolist (window (dimitern-windows/find-side-windows))
    (when (window-live-p window)
      (quit-window nil window)
      ;; When the window is still live, delete it
      (when (window-live-p window)
        (delete-window window)))))

(defun dimitern-windows/switch-to-minibuffer-window ()
  "Switch to current minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun dimitern-windows/toggle-current-window-dedication ()
  "Toggle dedication state of a window."
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(use-package default
  :bind
  ;; Standard window commands
  (("C-c w =" . balance-window)
   ("C-c w k" . delete-window)
   ("C-c w /" . split-window-right)
   ("C-c w -" . split-window-below)
   ("C-c w m" . delete-other-windows)
   ;; Additional custom bindings.
   ("C-c w q" . dimitern-windows/quit-all-side-windows)
   ("C-c w d" . dimitern-windows/toggle-current-window-dedication)
   ("C-c w b" . dimitern-windows/switch-to-minibuffer-window)))

;; windmove: move between windows with Shift+Arrow.
(use-package windmove
  :bind (("C-c w <left>"  . windmove-left)
         ("C-c w <right>" . windmove-right)
         ("C-c w <up>"    . windmove-up)
         ("C-c w <down>"  . windmove-down)))

;; winner: undo and redo window configurations.
(use-package winner
  :init
  (winner-mode))

;; ace-window: fast window switching.
(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)
         ("C-c w w" . ace-window)))

;; golden-ration: automatically resize windows.
(use-package golden-ratio
  :ensure t
  :preface
  (defun dimitern-windows/toggle-golden-ratio ()
    (interactive)
    (if (bound-and-true-p golden-ratio-mode)
        (progn
          (golden-ratio-mode -1)
          (balance-windows))
      (golden-ratio-mode)
      (golden-ratio)))
  :bind (("C-c t g" . dimitern-windows/toggle-golden-ratio))
  :config
  (validate-setq
   golden-ratio-extra-commands '(windmove-up
                                 windmove-down
                                 windmove-left
                                 windmove-right
                                 ace-window
                                 ace-delete-window
                                 ace-select-window
                                 ace-swap-window
                                 ace-maximize-window)
   ;; Exclude a couple of special modes from golden ratio, namely
   ;; Flycheck's error list, calc
   golden-ratio-exclude-modes '(flycheck-error-list-mode
                                calc-mode
                                dired-mode
                                ediff-mode
                                )
   golden-ratio-exclude-buffer-regexp
   `(,(rx bos "*which-key*" eos)
     ,(rx bos "*NeoTree*" eos)))
  :diminish (golden-ratio-mode . "ⓖ"))

;; ediff-wind: Ediff window management.
(use-package ediff-wind
  :defer t
  :config
  ;; Prevent Ediff from spamming the frame
  (validate-setq
   ediff-window-setup-function #'ediff-setup-windows-plain
   ediff-split-window-function #'split-window-horizontally))

(provide 'dimitern-windows)
