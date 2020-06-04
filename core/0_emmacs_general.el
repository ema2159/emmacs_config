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

;; Delete selection mode
(delete-selection-mode 1)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable scrollbar
(scroll-bar-mode -1)

;; Disable menubar
(menu-bar-mode -1)

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
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Function for saving and killing buffer and window
(defun save-and-kill-buffer-and-window ()
  "Function for saving buffer and closing it with its window, used for evil mode :wq command."
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

;; Configure backup files
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Tramp cofiguration
(setq tramp-default-mode "ssh")
(customize-set-variable
 'tramp-ssh-controlmaster-options
 (concat
  "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
  "-o ControlMaster=auto -o ControlPersist=yes"))
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
	      vc-ignore-dir-regexp
	      tramp-file-name-regexp))
(setq tramp-auto-save-directory "~/emacs/tramp-autosave")
(setq tramp-verbose 3)

;; Switch to help buffer automatically
(setq help-window-select t)

;; Remove the side fringes
;; (fringe-mode 0)
(setq left-fringe-width  20)
(setq right-fringe-width 0)

;; Default isearch to forward
(setq isearch-forward t)

;; Focus most recent window
(global-set-key (kbd "C-x 2") (lambda () (interactive) (split-window-vertically) (other-window 1)))
(global-set-key (kbd "C-x 3") (lambda () (interactive) (split-window-horizontally) (other-window 1)))

;; Default font
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-frame-font "Consolas" nil t)
(set-face-attribute 'default nil :height 130)

;; Which func config
(setq which-func-unknown "n/a")

;; Smooth scrolling
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

(provide '0_emmacs_general)
;;; 0_emmacs_general.el ends here
