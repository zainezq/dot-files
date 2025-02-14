;;; shells.el --- Shell setup for Emacs -*- lexical-binding: t; -*-

;; Author: Zaine
;; Version: 1.0
;; URL: https://github.com/zainezq/dot-files/

;;; Commentary:

;; el file for shell config

;;; Code:

;; ----------------------------------------------------------------------------
;; Eshell Toggle Configuration
;; ----------------------------------------------------------------------------

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term))

;; ----------------------------------------------------------------------------
;; Eshell Syntax Highlighting Configuration
;; ----------------------------------------------------------------------------

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

;; ----------------------------------------------------------------------------
;; Vterm Configuration
;; ----------------------------------------------------------------------------

(use-package vterm
  :config
  (setq shell-file-name "/bin/zsh"
        vterm-max-scrollback 5000))

;; ----------------------------------------------------------------------------
;; Vterm Toggle Configuration
;; ----------------------------------------------------------------------------

(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer)))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  (reusable-frames . visible)
                  (window-height . 0.3))))

;; ----------------------------------------------------------------------------
;; Provide
;; ----------------------------------------------------------------------------

(provide 'shells)

;;; shells.el ends here
