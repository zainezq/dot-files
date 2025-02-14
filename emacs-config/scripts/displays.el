;;; displays.el --- Neotree and Dashboard Configuration

;; Author: Zaine
;; Version: 1.0
;; URL: https://github.com/zainezq/dot-files

;;; Commentary:
;; This file configures `neotree` for file tree navigation and `dashboard` for a
;; startup screen with recent files, agenda items, and project shortcuts.

;;; Code:

;; ----------------------------------------------------------------------------
;; Neotree Configuration
;; ----------------------------------------------------------------------------

(use-package neotree
  :config
  ;; Neotree settings
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 30
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action)

  ;; Truncate long file names in Neotree
  (add-hook 'neo-after-create-hook
            (lambda (_)
              (with-current-buffer (get-buffer neo-buffer-name)
                (setq truncate-lines t)
                (setq word-wrap nil)
                (make-local-variable 'auto-hscroll-mode)
                (setq auto-hscroll-mode nil))))

;; ----------------------------------------------------------------------------
;; Dashboard Configuration
;; ----------------------------------------------------------------------------

(use-package dashboard
  :ensure t
  :init
  ;; Set Dashboard as the initial buffer
  (setq initial-buffer-choice 'dashboard-open)

  ;; Dashboard appearance settings
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "EHHHHHHHHHHHHHH!")
  (setq dashboard-startup-banner 1)
  (setq dashboard-center-content t) ;; Center content horizontally
  (setq dashboard-vertically-center-content t) ;; Center content vertically

  ;; Dashboard items to display
  (setq dashboard-items '((recents . 10)  ;; Recent files
                          (agenda . 10)  ;; Agenda items
                          (projects . 5))) ;; Projects

  ;; Customize heading icons
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))

  ;; Set up Dashboard on startup
  :config
  (dashboard-setup-startup-hook))

;; Disable Org Agenda startup message
(setq org-agenda-inhibit-startup t)

;; ----------------------------------------------------------------------------
;; Provide
;; ----------------------------------------------------------------------------

(provide 'displays)

;;; displays.el ends here
