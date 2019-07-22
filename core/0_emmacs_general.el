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

;; Custom syntax highlighting
;; Create a font lock face for punctuation signs
(defvar font-lock-punctuation-face 'font-lock-punctuation-face)
(defface font-lock-punctuation-face
  '((t :inherit font-lock-keyword-face))
  "Face for highlighting punctuation signs.")
(set-face-attribute 'font-lock-punctuation-face nil :weight 'normal)

;; Improve python-mode syntax highlighting
(font-lock-add-keywords 'python-mode
  '(("-?\\b[0-9]+\\.?" . font-lock-constant-face)
    ("[\.\,\;\:\*\|\&\!\(\)\{\}\=\$\<\>\'\#\%\-\+]\\|\\]\\|\\[" . font-lock-punctuation-face)
    ("\\([A-Za-z][A-Za-z0-9_]*\\)[ \t\n]*\\((.*)\\)"
     (1 font-lock-function-name-face))))
;; Improve js2-mode syntax highlighting
(font-lock-add-keywords 'js2-mode
  '(("-?\\b[0-9]+\\.?" . font-lock-constant-face)
    ("[\.\,\;\:\*\|\&\!\(\)\{\}\=\$\<\>\'\#\%\-\+]\\|\\]\\|\\[" . font-lock-punctuation-face)))
;; Improve c-mode syntax highlighting
(font-lock-add-keywords 'c-mode
  '(("-?\\b[0-9]+\\.?" . font-lock-constant-face)
    ("[\.\,\;\:\*\|\&\!\(\)\{\}\=\$\<\>\'\#\%\-\+\@]\\|\\]\\|\\[" . font-lock-punctuation-face)))
;; Improve verilog-mode syntax highlighting
(font-lock-add-keywords 'verilog-mode
  '(("-?\\b[0-9]+\\.?" . font-lock-constant-face)
    ("[\.\,\;\:\*\|\&\!\(\)\{\}\=\$\<\>\'\#\%\-\+\@]\\|\\]\\|\\[" . font-lock-punctuation-face)))

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

;; desktop-save configuration
(setq desktop-path '("~/.emacs.d/sessions"))
(setq desktop-load-locked-desktop t)
(add-hook 'kill-emacs-hook '(lambda()
			      (desktop-save (car desktop-path))))

;; Remove the side fringes
(fringe-mode 0)

;; Default isearch to forward
(setq isearch-forward t)

(provide '0_emmacs_general)
;;; 0_emmacs_general.el ends here
