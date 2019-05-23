;;; 0_emmacs_general.el --- General configurations

;;; Commentary:
;; This section contains some general Emacs configuration and parameters

;;; Code:
;; When split, if a buffer is killed, also its window is killed
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)

;; Configure C-/ as comment keybinding
(global-set-key (kbd "C-7") 'comment-line)

;;Set default mode
(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; When poping a new buffer, split horizontally
(setq split-height-threshold 160)
(setq split-width-threshold nil)

;; Highlight matching braces
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; Narrow and widening configuration
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Delete selection mode
(delete-selection-mode 1)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable scrollbar
(scroll-bar-mode -1)

;; Remove warning bell sound
(setq ring-bell-function 'ignore)

;; Modify word wrap arrows
(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000000
   #b00010000
   #b00001000
   #b11111100
   #b00001000
   #b00010000
   #b00000000])
(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b00010000
   #b00001000
   #b11111100
   #b00001000
   #b00010000
   #b00000000
   #b00000000])

;; Define a variable for hooks to turn on/off the relative and absolute number lines
(defvar linum-active t)

;; Function for saving and killing buffer and window
(defun save-and-kill-buffer-and-window ()
  "Function for saving buffer and closing it with its window, used for evil mode :wq command."
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

(provide '0_emmacs_general)
;;; 0_emmacs_general.el ends here
