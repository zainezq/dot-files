(use-package general
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer dt/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode
  

  (dt/leader-keys
    "." '(find-file :wk "Find file")
    "f c" '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :wk "Edit emacs config")
    "TAB TAB" '(comment-line :wk "Comment lines")
    "<right>" '(next-buffer :wk "Next buffer")
    "<left>" '(previous-buffer :wk "Previous buffer"))


  (dt/leader-keys
    "b" '(:ignore t :wk "buffer")
    "b b" '(switch-to-buffer :wk "Switch buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-this-buffer :wk "Kill this buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(basic-save-buffer :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers")
    "b r" '(revert-buffer :wk "Reload buffer"))
    


  (dt/leader-keys
    "e" '(:ignore t :wk "Eshell/Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    ;;"e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e e" '(org-babel-execute-src-block :wk "evaluate code block")
    "e h" '(counsel-esh-history :which-key "Eshell history")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    "e s" '(eshell :which-key "Eshell"))
   
  (dt/leader-keys
    "h" '(:ignore t :wk "Help")
    "h f" '(describe-function :wk "Describe function")
    "h v" '(describe-variable :wk "Describe variable")
    "h k" '(describe-key :wk "describes the key")
    "h r r" '(reload-init-file :wk "Reload emacs config")
    "h s" '(isearch-forward :wk "Search the buffer"))

  (dt/leader-keys
    "t" '(:ignore t :wk "Toggle")
    ;; "t e" '(eshell-toggle :wk "Toggle eshell")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t n" '(neotree-toggle :wk "Toggle neotree file viewer")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
    "t v" '(vterm-toggle :wk "Toggle vterm"))


  (dt/leader-keys
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    "w 0" '(delete-window :wk "Delete the current window")
    "w 1" '(delete-other-windows :wk "Keep only current window")
    ;; Window motions
    ;;"w h" '(evil-window-left :wk "Window left")
    ;;"w j" '(evil-window-down :wk "Window down")
    ;;"w k" '(evil-window-up :wk "Window up")
    ;;"w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window"))
    ;; Move Windows
    ;;"w H" '(buf-move-left :wk "Buffer move left")
    ;;"w J" '(buf-move-down :wk "Buffer move down")
    ;;"w K" '(buf-move-up :wk "Buffer move up")
    ;;"w L" '(buf-move-right :wk "Buffer move right"))

  (dt/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(neotree-dir :wk "Open directory in neotree")
    "d p" '(peep-dired :wk "Peep-dired"))


  (dt/leader-keys
    "m" '(:ignore t :wk "Org")
    "m a" '(org-agenda :wk "Org agenda")
    "m e" '(org-export-dispatch :wk "Org export dispatch")
    "m i" '(org-toggle-item :wk "Org toggle item")
    "m t" '(org-todo :wk "Org todo")
    "m B" '(org-babel-tangle :wk "Org babel tangle")
    "m T" '(org-todo-list :wk "Org todo list"))

  (dt/leader-keys
    "m b" '(:ignore t :wk "Tables")
    "m b -" '(org-table-insert-hline :wk "Insert hline in table"))

  (dt/leader-keys
    "m d" '(:ignore t :wk "Date/deadline")
    "m d t" '(org-time-stamp :wk "Org time stamp"))

  (dt/leader-keys
    "p" '(projectile-command-map :wk "Projectile"))

  (dt/leader-keys
    "c" '(:ignore t :wk "controls")
    "c n l" '(org-roam-buffer-toggle :wk "Toggle the buffer")
    "c n f" '(org-roam-node-find :wk "Find a node")
    "c n i" '(org-roam-node-insert :wk "Insert a new node")
    "c i b" '(insert-new-book-entry :wk "Insert new book entry")
    "c i c" '(insert-commit-template :wk "Insert commit template")
))


(use-package dired-open
    :config
    (setq dired-open-extensions '(("gif" . "sxiv")
                                  ("jpg" . "sxiv")
                                  ("png" . "sxiv")
                                  ("mkv" . "mpv")
                                  ("mp4" . "mpv"))))

(use-package peep-dired
    :after dired
    :hook (evil-normalize-keymaps . peep-dired-hook)
    :config
    (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
    (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
    (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
    (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
    )



(use-package which-key
  :init
    (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
	  which-key-sort-order #'which-key-key-order-alpha
	  which-key-allow-imprecise-window-fit nil
	  which-key-sort-uppercase-first nil
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.8
	  which-key-max-description-length 25
	  which-key-allow-imprecise-window-fit nil
	  which-key-separator " → " ))

(provide 'keyboard)

