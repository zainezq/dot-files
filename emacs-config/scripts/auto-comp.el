;;; auto-comp.el --- Autocompletion and Project Management Configuration

;; Author: Zaine
;; Version: 1.0
;; URL: https://github.com/yourusername/your-repo

;;; Commentary:
;; This file configures autocompletion, project management, and navigation tools
;; such as `company`, `irony`, `projectile`, `flycheck`, and `ivy` for enhanced
;; productivity in Emacs.

;;; Code:

;; ----------------------------------------------------------------------------
;; Autocompletion (Company Mode and Irony)
;; ----------------------------------------------------------------------------

;; Make sure to install irony
(require 'irony)

;; Enable company-mode globally for autocompletion
(add-hook 'after-init-hook 'global-company-mode)

;; Enable irony-mode for C/C++ mode
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)

;; Enable company-irony for C/C++ autocompletion
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; Enable flycheck for on-the-fly syntax checking
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Enable yasnippet for snippet support
(yas-global-mode 1)

;; Configure Irony mode
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Optional: Advanced Company Mode Configuration
;; (use-package company
;;   :defer 2
;;   :diminish
;;   :custom
;;   (company-begin-commands '(self-insert-command))
;;   (company-idle-delay .1)
;;   (company-minimum-prefix-length 2)
;;   (company-show-numbers t)
;;   (company-tooltip-align-annotations 't)
;;   (global-company-mode t))

;; Optional: Company Box for a prettier UI
;; (use-package company-box
;;   :after company
;;   :diminish
;;   :hook (company-mode . company-box-mode))

;; ----------------------------------------------------------------------------
;; Project Management (Projectile)
;; ----------------------------------------------------------------------------

(use-package projectile
  :config
  (projectile-mode 1))

;; Optional: Diminish mode to reduce clutter in the mode line
;; (use-package diminish)

;; ----------------------------------------------------------------------------
;; Syntax Checking (Flycheck)
;; ----------------------------------------------------------------------------

;; Optional: Flycheck configuration
;; (use-package flycheck
;;   :ensure t
;;   :defer t
;;   :diminish
;;   :init (global-flycheck-mode))

;; ----------------------------------------------------------------------------
;; Navigation and Completion (Ivy, Counsel, and Ivy-Rich)
;; ----------------------------------------------------------------------------

;; Ivy and Counsel for enhanced completion and navigation
(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :bind
  ;; Resume the last Ivy-based completion
  (("C-c C-r" . ivy-resume)
   ;; Switch buffers in another window
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode 1))

;; Ivy-Rich for enhanced buffer descriptions
(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1) ;; Enable descriptions in M-x
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev))

;; ----------------------------------------------------------------------------
;; Provide
;; ----------------------------------------------------------------------------

(provide 'auto-comp)

;;; auto-comp.el ends here
