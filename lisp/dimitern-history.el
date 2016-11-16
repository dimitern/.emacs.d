;; dimitern-history.el: History tweaks.
;;

;; From http://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html:
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(provide 'dimitern-history)
