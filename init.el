;package
(require 'package)
;;add to list
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;scripts load
(add-to-list 'load-path "~/.emacs.d/scripts/")
(require 'displays) ;;opens displays
(require 'shells)
(require 'auto-comp)
(require 'window)
(require 'keyboard)
(require 'custom-agenda)

;;remove startup screen
(setq inhibit-startup-screen t)

;; exit munubuffer via escape
(global-set-key [escape] 'keyboard-escape-quit)

;;ORG ------------------------------------------------
;;ORG MODE AGENDA
(setq org-agenda-files '("~/master-folder/org_files/master1.org"
			 "~/master-folder/org_files/deen-org/deen.org"
			 "~/master-folder/org_files/career-org/career.org"))
(require 'org)


;;images
(add-hook 'org-mode-hook 'org-display-inline-images)
(add-hook 'org-mode-hook
          (lambda ()
            (org-display-inline-images t t)))

;;Enabling Table of Contents
(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

;;Enabling Org Bullets
(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;Disable Electric Indent
(electric-indent-mode -1)

;;roam config

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/master-folder/org_files/org_roam")
  :config
  (org-roam-setup))

;; Source Code Block Tag Expansion
(require 'org-tempo)

;; all the icons - - - - - -- - - -----------------------------------------
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))
		     
;;Disable Menubar, Toolbars and Scrollbars
(menu-bar-mode -1)
;;(tool-bar-mode -1)
(scroll-bar-mode -1)

;;Display Line Numbers and Truncated Lines
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

;; reload emacs
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))

;;---------------------------------------------------------------------------------------------------
;;THEMES
 (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(require 'kaolin-themes)
 (load-theme 'kaolin-bubblegum t)
;;themes
;;(use-package badger-theme
;;  :ensure t
;;  :config (load-theme 'badger t))

;;languages
(use-package haskell-mode)
;;(use-package lua-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((haskell . t)))  ;; Enable Haskell


;;backup
(setq backup-directory-alist '((".*" . "~/.Trash")))

(use-package evil
    :init      ;; tweak evil's configuration before loading it
    (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
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


(defun kill-other-buffers-startup ()
  "Kill all buffers except the current one."
  (mapc (lambda (buffer)
          (unless (eq buffer (current-buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(add-hook 'emacs-startup-hook #'kill-other-buffers-startup)


(defun find-file-root (FILENAME)
  "Edit a file as root."
  (interactive "FFind file: ")
  (find-file (concat "/sudo::" FILENAME)))

provide ('init)
;;; init.el ends here

