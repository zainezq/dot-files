;;company ----------------------------------------------------

;;autoc
(require 'irony)

;; Enable company-mode globally for autocompletion
(add-hook 'after-init-hook 'global-company-mode)

;; Enable irony-mode for C/C++ mode
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)

;; Enable company-irony
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; Enable flycheck for on-the-fly syntax checking
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Enable yasnippet for snippet support
(yas-global-mode 1)

;; Irony mode settings
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


;;(use-package company
;;  :defer 2
;;  :diminish
;;  :custom
;;  (company-begin-commands '(self-insert-command))
;;  (company-idle-delay .1)
  ;;(company-minimum-prefix-length 2)
  ;;(company-show-numbers t)
;;  (company-tooltip-align-annotations 't)
;;  (global-company-mode t))

;;(use-package company-box
;;  :after company
;;  :diminish
;;  :hook (company-mode . company-box-mode))


;;projectile
(use-package projectile
  :config
  (projectile-mode 1))

;;diminish
;;(use-package diminish)

;;flycheck
;;(use-package flycheck
  ;;:ensure t
  ;;:defer t
  ;;:diminish
  ;;:init (global-flycheck-mode))

;;ivy ----------------------------------------------------
(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :bind
  ;; ivy-resume resumes the last Ivy-based completion.
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev))
(provide 'auto-comp)
