;;; 3_emmacs_tools.el --- Tools

;;; Commentary:
;; In this section the following packages are loaded:
;; - Avy
;; - Beacon
;; - Column Enforce Mode
;; - Company
;; - Company C Headers
;; - Company Irony
;; - Company Tern
;; - Drag Stuff
;; - Dumb Jump
;; - Evil
;; - Evil Matchit
;; - Evil Multiedit
;; - Evil Snipe
;; - Expand Region
;; - Flycheck
;; - Hydra
;; - Highlight Thing
;; - Magit
;; - Projectile
;; - Recentf
;; - Undo Tree
;; - Shell Pop
;; - Smartparens
;; - Windmove
;; - Yasnippet

;;; Code:
;; Evil
(use-package evil
  :ensure t
  :init
  (evil-mode)
  :config
  ;; When evil :q[uit], close buffer and window instead of Emacs
  (evil-ex-define-cmd "q[uit]" 'kill-buffer-and-window)
  ;; When evil :wq, save and close buffer and window instead of Emacs
  (evil-ex-define-cmd "wq" 'save-and-kill-buffer-and-window)
  ;; Remap "," to repeat last macro (@@)
  (define-key evil-normal-state-map "," (kbd "@@"))
  (setq isearch-forward t)
  :bind (:map evil-normal-state-map
	      ("/" . swiper)
	      ("?" . swiper)))

;; Dumb Jump
(use-package dumb-jump
  :ensure t
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-prefer-searcher 'rg)
  (dumb-jump-mode)
  :bind (:map evil-motion-state-map
	      ("gd" . dumb-jump-go)
	      ("gb" . dumb-jump-back)
	      ("go" . dumb-jump-go-other-window)
	      ("gl" . dumb-jump-quick-look)))

;; Drag Stuff
(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-define-keys)
  :hook
  (prog-mode . drag-stuff-mode))

;; Evil Multiedit
(use-package evil-multiedit
  :ensure t
  :bind (:map evil-normal-state-map
	 ("C-<" . evil-multiedit-match-symbol-and-next)
	 ("C->" . evil-multiedit-match-symbol-and-prev)
	 :map evil-visual-state-map
	 ("C-<" . evil-multiedit-match-and-next)
	 ("C->" . evil-multiedit-match-and-prev)
	 ("R" . evil-multiedit-match-all)
	 ("C-M-D" . evil-multiedit-restore)
	 :map evil-insert-state-map
	 ("C-<" . evil-multiedit-match-and-next)
	 ("C->" . evil-multiedit-match-and-prev)
	 :map evil-motion-state-map
	 ("RET" . evil-multiedit-toggle-or-restrict-region)
	 :map evil-multiedit-state-map
	 ("RET" . evil-multiedit-toggle-or-restrict-region)
	 ("C-n" . evil-multiedit-next)
	 ("C-p" . evil-multiedit-prev)
	 :map evil-multiedit-insert-state-map
	 ("C-n" . evil-multiedit-next)
	 ("C-p" . evil-multiedit-prev))
  :config
  (evil-ex-define-cmd "ie[dit]" #'evil-multiedit-ex-match))

;; Recent files
(use-package recentf
  :ensure t)

;; Magit
(use-package magit
  :bind ("C-x g" . magit-status)
  :init
  (add-hook 'magit-process-mode-hook
	    (lambda () (setq-local linum-active nil))))

;; Avy
(use-package avy
  :bind* ("C-'" . avy-goto-word-1)
  :config
  (avy-setup-default)
  (setq avy-background t))

;; Beacon
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

;; Windmove
(use-package windmove
  :ensure t
  :after hydra
  :config
  (defhydra hydra-window (global-map "C-x"
				     :color red
				     :hint nil)
    ("C-<left>"  windmove-left)
    ("C-<right>" windmove-right)
    ("C-<up>"    windmove-up)
    ("C-<down>"  windmove-down)
    ("2" split-window-below)
    ("3" split-window-right)
    ("1" delete-other-windows)))

;; Undo Tree
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  (setq undo-tree-enable-undo-in-region nil)
  :hook
  (undo-tree-visualizer-mode . transpose-frame))

;; Smart parens
(use-package smartparens
  :ensure t
  :hook
  (python-mode . smartparens-mode)
  (emacs-lisp-mode . smartparens-mode)
  (c-mode . smartparens-mode)
  (c++-mode . smartparens-mode)
  (specman-mode . smartparens-mode)
  (js2-mode . smartparens-mode)
  (html-mode . smartparens-mode))

;; Highlight indent guides
(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'stack)
  :hook
  (prog-mode-hook . highlight-indent-guides-mode))

;; Flycheck
(use-package flycheck
  :ensure t
  :defer 3
  :init
  (global-flycheck-mode))

;; Expand Region
(use-package expand-region
  :ensure t
  :bind
  ("C-0"  . 'er/expand-region))

;; Highlight thing
(use-package highlight-thing
  :ensure t
  :config
  (set-face-attribute 'highlight-thing nil
		      :inherit 'highlight)
  (add-hook 'iedit-mode-hook (lambda()
			       (highlight-thing-mode -1)))

  (add-hook 'iedit-mode-end-hook (lambda()
				   (highlight-thing-mode 1)))
  (add-hook 'evil-visual-state-entry-hook (lambda()
			       (highlight-thing-mode -1)))
  (add-hook 'evil-visual-state-exit-hook (lambda()
			       (highlight-thing-mode 1)))
  :init
  (setq highlight-thing-what-thing 'symbol)
  (setq highlight-thing-delay-seconds 0.5)
  (setq highlight-thing-all-visible-buffers-p t)
  :hook
  (prog-mode . highlight-thing-mode)
  (specman-mode . highlight-thing-mode))

;; Column Enforce Mode
(use-package column-enforce-mode
  :ensure t
  :hook
  (prog-mode . column-enforce-mode)
  :config
  (setq column-enforce-column 100))

;; Shell-pop
(use-package shell-pop
  :ensure t
  :config
  (setq shell-pop-window-size 30)
  (evil-set-initial-state 'term-mode 'emacs)
  :bind ("<f5>" . shell-pop))

;; Evil Snipe
(use-package evil-snipe
  :ensure t
  :hook
  (prog-mode . evil-snipe-mode)
  (magit-mode . turn-off-evil-snipe-override-mode))

;; Company Tern
(use-package company-tern
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-tern))

;; Company C headers
(use-package company-c-headers
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers))

;; Company Irony
(use-package company-irony
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-irony))

;; Company
(use-package company
  :ensure t
  :defer 5
  :hook
  (after-init . global-company-mode)
  :config
  (company-mode 1)
  (setq company-idle-delay 0))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :defer t
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets" ;; personal snippets
	  ))
  ;; Add yasnippet support for all company backends
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or
	 (not company-mode/enable-yas)
	 (and (listp backend)
	      (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
	      '(:with company-yasnippet))))
  (setq company-backends
	(mapcar #'company-mode/backend-with-yas company-backends))
  (yas-global-mode 1)
  (setq require-final-newline nil)

  ;; Yas auxiliar functions
  (defun if-yas-empty ()
    (interactive)
    (if (and yas-moving-away-p (not yas-modified-p))
	(yas-clear-field))))

;; Ivy yasnippet
(use-package ivy-yasnippet
  :ensure t
  :bind
  ("C-x C-a" . ivy-yasnippet))

(use-package hydra
  :ensure t
  :defer 2
  :config
  (hydra-add-font-lock))

(use-package evil-easymotion
  :ensure t
  :after evil
  :config
  (evilem-default-keybindings "SPC")
  (evilem-define (kbd "SPC <up>") 'evil-previous-line)
  (evilem-define (kbd "SPC <down>") 'evil-next-line))

(use-package projectile
  :ensure t
  :config
  (defadvice projectile-on (around exlude-tramp activate)
    "This should disable projectile when visiting a remote file"
    (unless  (--any? (and it (file-remote-p it))
		     (list
		      (buffer-file-name)
		      list-buffers-directory
		      default-directory
		      dired-directory))
      ad-do-it))
  (progn
    (setq projectile-file-exists-remote-cache-expire nil)
    (add-hook 'find-file-hook
	      (lambda ()
		(when (file-remote-p default-directory)
		  (setq-local projectile-mode-line "Projectile"))))
    (setq projectile-completion-system 'ivy)
    ;; (setq projectile-mode-line '(:eval (format " Projectile[%s]" (projectile-project-name))))
    (setq projectile-globally-ignored-directories
	  (quote
	   (".idea" ".eunit" ".git" ".hg" ".svn" ".fslckout" ".bzr" "_darcs" ".tox" "build" "target"))))
  (progn
    (projectile-mode 1)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

;; Evil Matchit
(use-package evil-matchit
  :ensure t
  :after evil
  :hook
  (html-mode . turn-on-evil-matchit-mode))

(provide '3_emmacs_tools)
;;; 3_emmacs_tools.el ends here
