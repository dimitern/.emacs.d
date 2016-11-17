;; dimitern-history.el: History tweaks.
;;

;; From http://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html:
(setq
 savehist-file "~/.emacs.d/savehist"
 history-length t
 history-delete-duplicates t
 savehist-save-minibuffer-history 1
 savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring)
)

(savehist-mode 1)

(provide 'dimitern-history)
