;;; keyboard.el --- Custom Keyboard Shortcuts for Emacs

;; Author: Zaine
;; Version: 1.0
;; URL: https://github.com/zainezq/dot-files/

;;; Commentary:

;; el file for the keyboard bindings

;;; Code:

;; ----------------------------------------------------------------------------
;; General Keybindings Configuration
;; ----------------------------------------------------------------------------

(use-package general
  :config
  ;; Integrate with Evil mode
  (general-evil-setup)

  ;; Set up 'SPC' as the global leader key
  (general-create-definer dt/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; Set leader key
    :global-prefix "M-SPC") ;; Access leader key in insert mode

  ;; Leader keybindings
  (dt/leader-keys
    "." '(find-file :wk "Find file")
    "f c" '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :wk "Edit Emacs config")
    "TAB TAB" '(comment-line :wk "Comment lines")
    "<right>" '(next-buffer :wk "Next buffer")
    "<left>" '(previous-buffer :wk "Previous buffer"))

  ;; Buffer management
  (dt/leader-keys
    "b" '(:ignore t :wk "Buffer")
    "b b" '(switch-to-buffer :wk "Switch buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-this-buffer :wk "Kill this buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(basic-save-buffer :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers")
    "b r" '(revert-buffer :wk "Reload buffer"))

  ;; Evaluation and Eshell
  (dt/leader-keys
    "e" '(:ignore t :wk "Eshell/Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(org-babel-execute-src-block :wk "Evaluate code block")
    "e h" '(counsel-esh-history :wk "Eshell history")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    "e s" '(eshell :wk "Eshell"))

  ;; Help and Search
  (dt/leader-keys
    "h" '(:ignore t :wk "Help")
    "h f" '(describe-function :wk "Describe function")
    "h v" '(describe-variable :wk "Describe variable")
    "h k" '(describe-key :wk "Describe key")
    "h r r" '(reload-init-file :wk "Reload Emacs config")
    "h s" '(isearch-forward :wk "Search the buffer"))

  ;; Toggles
  (dt/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t n" '(neotree-toggle :wk "Toggle neotree file viewer")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
    "t v" '(vterm-toggle :wk "Toggle vterm"))

  ;; Window Management
  (dt/leader-keys
    "w" '(:ignore t :wk "Windows")
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    "w 0" '(delete-window :wk "Delete the current window")
    "w 1" '(delete-other-windows :wk "Keep only current window")
    "w w" '(evil-window-next :wk "Goto next window"))

  ;; Dired and File Management
  (dt/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(neotree-dir :wk "Open directory in neotree")
    "d p" '(peep-dired :wk "Peep-dired"))

  ;; Org Mode
  (dt/leader-keys
    "m" '(:ignore t :wk "Org")
    "m a" '(org-agenda :wk "Org agenda")
    "m e" '(org-export-dispatch :wk "Org export dispatch")
    "m i" '(org-toggle-item :wk "Org toggle item")
    "m t" '(org-todo :wk "Org todo")
    "m B" '(org-babel-tangle :wk "Org babel tangle")
    "m T" '(org-todo-list :wk "Org todo list"))

  ;; Org Tables
  (dt/leader-keys
    "m b" '(:ignore t :wk "Tables")
    "m b -" '(org-table-insert-hline :wk "Insert hline in table"))

  ;; Org Dates and Deadlines
  (dt/leader-keys
    "m d" '(:ignore t :wk "Date/Deadline")
    "m d t" '(org-time-stamp :wk "Org time stamp"))

  ;; Projectile
  (dt/leader-keys
    "p" '(projectile-command-map :wk "Projectile"))

  ;; Controls and Insertions
  (dt/leader-keys
    "c" '(:ignore t :wk "Controls")
    "c n l" '(org-roam-buffer-toggle :wk "Toggle the buffer")
    "c n f" '(org-roam-node-find :wk "Find a node")
    "c n i" '(org-roam-node-insert :wk "Insert a new node")
    "c i b" '(insert-new-book-entry :wk "Insert new book entry")
    "c i c" '(insert-commit-template :wk "Insert commit template")))

;; ----------------------------------------------------------------------------
;; Dired Configuration
;; ----------------------------------------------------------------------------

(use-package dired-open
  :config
  (setq dired-open-extensions '(("gif" . "sxiv")
                                ("jpg" . "sxiv")
                                ("png" . "sxiv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

;; ----------------------------------------------------------------------------
;; Peep-Dired Configuration
;; ----------------------------------------------------------------------------

(use-package peep-dired
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  :config
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file)
  (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
  (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file))

;; ----------------------------------------------------------------------------
;; Which-Key Configuration
;; ----------------------------------------------------------------------------

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
        which-key-separator " → "))

;; ----------------------------------------------------------------------------
;; Provide
;; ----------------------------------------------------------------------------

(provide 'keyboard)

;;; keyboard.el ends here
