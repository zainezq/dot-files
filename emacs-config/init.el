;;; init.el --- Zaine's Emacs Configuration

;; Author: Zaine
;; Version: 1.0
;; URL: https://github.com/zainezq/dot-files/emacs-config

;;; Commentary:

;; Just my Emacs config

;;; Code:

;; ----------------------------------------------------------------------------
;; Package Management
;; ----------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; ----------------------------------------------------------------------------
;; Custom Scripts
;; ----------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/scripts/")
(require 'displays)  ;; Open displays
(require 'shells)
(require 'auto-comp)
(require 'window)
(require 'keyboard)
(require 'custom-agenda)

;; ----------------------------------------------------------------------------
;; General Settings
;; ----------------------------------------------------------------------------

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Exit minibuffer with Escape
(global-set-key [escape] 'keyboard-escape-quit)

;; ----------------------------------------------------------------------------
;; Org Mode Configuration
;; ----------------------------------------------------------------------------

;; Org Mode Agenda Files
;; Modify the file names as needed
(setq org-agenda-files '("~/path/to/1.org"
                         "~/path/to/2.org"
                         "~/path/to/n.org"))
(require 'org)

;; Display inline images in Org Mode
(add-hook 'org-mode-hook 'org-display-inline-images)
(add-hook 'org-mode-hook
          (lambda ()
            (org-display-inline-images t t)))

;; Enable Table of Contents in Org Mode
(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

;; Enable Org Bullets
(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Disable Electric Indent
(electric-indent-mode -1)

;; Org Roam Configuration
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/master-folder/org_files/org_roam")
  :config
  (org-roam-setup))

;; Source Code Block Tag Expansion
(require 'org-tempo)

;; ----------------------------------------------------------------------------
;; UI Enhancements
;; ----------------------------------------------------------------------------

;; All the Icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

;; Disable Menubar, Toolbars, and Scrollbars
(menu-bar-mode -1)
;; (tool-bar-mode -1)  ;; Uncomment if you want to disable the toolbar
(scroll-bar-mode -1)

;; Display Line Numbers and Truncated Lines
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

;; Reload Emacs Configuration
(defun reload-init-file ()
  "Reload the Emacs configuration file."
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))

;; ----------------------------------------------------------------------------
;; Themes
;; ----------------------------------------------------------------------------

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(require 'kaolin-themes)
(load-theme 'kaolin-bubblegum t)

;; ----------------------------------------------------------------------------
;; Language Support
;; ----------------------------------------------------------------------------

(use-package haskell-mode)
;; (use-package lua-mode)  ;; Uncomment if you need Lua support

(org-babel-do-load-languages
 'org-babel-load-languages
 '((haskell . t)))

;; ----------------------------------------------------------------------------
;; Backup Settings
;; ----------------------------------------------------------------------------

(setq backup-directory-alist '((".*" . "~/.Trash")))

;; ----------------------------------------------------------------------------
;; Evil Mode Configuration
;; ----------------------------------------------------------------------------

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))

(use-package evil-tutor)

;; ----------------------------------------------------------------------------
;; Utility Functions
;; ----------------------------------------------------------------------------

(defun kill-other-buffers-startup ()
  "Kill all buffers except the current one."
  (interactive)
  (mapc (lambda (buffer)
          (unless (eq buffer (current-buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(add-hook 'emacs-startup-hook #'kill-other-buffers-startup)

(defun find-file-root (FILENAME)
  "Edit a file as root."
  (interactive "FFind file: ")
  (find-file (concat "/sudo::" FILENAME)))

;; ----------------------------------------------------------------------------
;; Provide
;; ----------------------------------------------------------------------------

(provide 'init)

;;; init.el ends here
