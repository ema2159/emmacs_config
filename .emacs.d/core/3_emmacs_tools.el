;;; 3_emmacs_tools.el --- Tools

;;; Commentary:
;; In this section the following packages are loaded:
;; - Avy
;; - Beacon
;; - Column Enforce Mode
;; - Company
;; - Drag Stuff
;; - Dumb Jump
;; - Evil
;; - Evil Multiedit
;; - Expand Region
;; - Flycheck
;; - Highlight Thing
;; - Magit
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
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

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
  :ensure t
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  :bind (("\C-x\ \C-r" . recentf-open-files)))

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
  :bind
  ("C-x C-<left>"  . windmove-left)
  ("C-x C-<right>" . windmove-right)
  ("C-x C-<up>"    . windmove-up)
  ("C-x C-<down>"  . windmove-down))

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
  (specman-mode . smartparens-mode))

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
  :hook
  ((flycheck-error-list-mode . (lambda () (setq-local linum-active nil))))
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
		      :inherit 'highlight :background "#494949")
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
  (prog-mode . column-enforce-mode))

;; Shell-pop
(use-package shell-pop
  :ensure t
  :config
  ;; Disable evil mode in term mode
  (evil-set-initial-state 'term-mode 'emacs)
  :hook
  (term-mode . (lambda()
		 (load-theme-buffer-local 'cyberpunk (current-buffer)))))

;; Company
(use-package company
  :ensure t
  :defer 5
  :init
  (company-mode 1)
  (add-hook 'after-init-hook 'global-company-mode)
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-c-headers)))

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

;; (use-package projectile
;;     :ensure t
;;     :init
;;     (progn
;;         (setq projectile-file-exists-remote-cache-expire nil)
;;         (setq projectile-mode-line '(:eval (format " Projectile[%s]" (projectile-project-name))))
;;         (setq projectile-globally-ignored-directories
;;             (quote
;;                 (".idea" ".eunit" ".git" ".hg" ".svn" ".fslckout" ".bzr" "_darcs" ".tox" "build" "target"))))
;;     :config
;;     (progn
;;         (projectile-mode 1)
;;         (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; ))

(provide '3_emmacs_tools)
;;; 3_emmacs_tools.el ends here
