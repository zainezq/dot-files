;;; window.el --- Easily swap windows in Emacs -*- lexical-binding: t; -*-

;; Author: Zaine
;; Version: 1.0
;; URL: https://github.com/zainezq/dot-files/

;;; Commentary:

;; This package provides convenient functions for swapping buffers between windows.
;; It allows users to move buffers up, down, left, or right across window splits.
;; The functions rely on `windmove` for navigation.

;;; Code:

(require 'windmove)
(require 'window)

;; ----------------------------------------------------------------------------
;; Buffer Movement Functions
;; ----------------------------------------------------------------------------

;;;###autoload
(defun buf-move-up ()
  "Swap the current buffer and the buffer above the split.
If there is no split, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up))
         (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window above this one")
      ;; Swap buffers
      (set-window-buffer (selected-window) (window-buffer other-win))
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-down ()
  "Swap the current buffer and the buffer below the split.
If there is no split, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
         (buf-this-buf (window-buffer (selected-window))))
    (if (or (null other-win)
            (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
        (error "No window below this one")
      ;; Swap buffers
      (set-window-buffer (selected-window) (window-buffer other-win))
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-left ()
  "Swap the current buffer and the buffer on the left of the split.
If there is no split, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
         (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window to the left")
      ;; Swap buffers
      (set-window-buffer (selected-window) (window-buffer other-win))
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-right ()
  "Swap the current buffer and the buffer on the right of the split.
If there is no split, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
         (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window to the right")
      ;; Swap buffers
      (set-window-buffer (selected-window) (window-buffer other-win))
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;; ----------------------------------------------------------------------------
;; Provide
;; ----------------------------------------------------------------------------

(provide 'window)

;;; window.el ends here
