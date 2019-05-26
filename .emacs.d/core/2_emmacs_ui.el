;;; 2_emmacs-ui.el --- User Insterface

;;; Commentary:
;; In this section the following packages are loaded:
;; - All The Icons Ivy
;; - Counsel
;; - Dashboard
;; - Dired Hacks
;; - Dired Single
;; - Ivy
;; - Smex
;; - Solaire Mode
;; - Swiper
;; - Tabbar
;; - Telephone Line
;; - Treemacs
;; - Which key

;;; Code:
;; FUNCTIONS
(defun my-dired-init ()
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
    'dired-single-buffer-mouse) 
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer "..")))))

;; All The Icons Ivy
(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup))

;; Solaire mode
(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

;; Dired
(use-package dired
  :defer t
  :after evil
  :config
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  :hook
  (dired-mode .
	      (lambda ()
		(dired-hide-details-mode 1))))

;; Dired Single
(use-package dired-single
  :ensure t
  :hook
  (dired-load-hook . my-dired-init))

;; All The Icons Dired
(if (display-graphic-p)
    (progn
      ;; if graphic
      (use-package all-the-icons)
      (use-package all-the-icons-dired
	:hook (dired-mode . treemacs-icons-dired-mode))))

;; Dired Hacks
(use-package dired-hacks-utils
  :ensure t
  :defer t )
(use-package dired-filter
  :ensure t
  :defer t
  :config
  (define-key dired-mode-map (kbd "C-f") dired-filter-mark-map))
(use-package dired-subtree
  :ensure t
  :defer t
  :config
  (setq dired-subtree-use-backgrounds nil)
  (define-key dired-mode-map (kbd "i") 'dired-subtree-insert)
  (define-key dired-mode-map (kbd "r") 'dired-subtree-remove))

;; Treemacs configuration
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-display-in-side-window     t
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-git-command-pipe           ""
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-max-git-entries            5000
          treemacs-no-png-images              nil
          treemacs-no-delete-other-windows    t
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-cursor                nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
	([f8]        . treemacs)
        ("C-x t ?"   . treemacs-helpful-hydra)
        ("C-x t d"   . treemacs-root-down)
        ("C-x t u"   . treemacs-root-up)
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t f" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

;; (use-package treemacs-projectile
;;   :after treemacs projectile
;;   :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;; Telephone line
(use-package telephone-line
  :ensure t
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-sin-left
	telephone-line-secondary-left-separator 'telephone-line-sin-hollow-left
	telephone-line-primary-right-separator 'telephone-line-sin-right
	telephone-line-secondary-right-separator 'telephone-line-sin-hollow-right)
  (telephone-line-mode 1)
  (set-face-attribute 'telephone-line-evil-normal
		      nil
		      :background
		      "#317DC9"))

;; Tabbar ruler
(defun tabbar-buffer-groups ()
  (list
   (cond
    ((or (eq major-mode 'magit-log-mode)
	 (eq major-mode 'magit-mode)
	 (eq major-mode 'magit-wip-mode)
	 (eq major-mode 'magit-blob-mode)
	 (eq major-mode 'magit-diff-mode)
	 (eq major-mode 'magit-file-mode)
	 (eq major-mode 'magit-refs-mode)
	 (eq major-mode 'magit-blame-mode)
	 (eq major-mode 'magit-popup-mode)
	 (eq major-mode 'magit-stash-mode)
	 (eq major-mode 'magit-cherry-mode)
	 (eq major-mode 'magit-reflog-mode)
	 (eq major-mode 'magit-status-mode)
	 (eq major-mode 'magit-process-mode)
	 (eq major-mode 'magit-stashes-mode)
	 (eq major-mode 'magit-repolist-mode)
	 (eq major-mode 'magit-revision-mode)
	 (eq major-mode 'magit-log-select-mode)
	 (eq major-mode 'magit-popup-help-mode)
	 (eq major-mode 'magit-auto-revert-mode)
	 (eq major-mode 'magit-merge-preview-mode)
	 (eq major-mode 'magit-submodule-list-mode)
	 (eq major-mode 'magit-wip-after-save-mode)
	 (eq major-mode 'magit-blame-read-only-mode)
	 (eq major-mode 'magit-wip-after-apply-mode)
	 (eq major-mode 'magit-wip-before-change-mode)
	 (eq major-mode 'magit-wip-initial-backup-mode)
	 (eq major-mode 'magit-wip-after-save-mode))
     "Magit")
    ((eq major-mode 'dired-mode)
     "Dired")
    ((eq major-mode 'dashboard-mode)
     "Dashboard")
    ((eq major-mode 'specman-mode)
     "Specman")
    ((eq major-mode 'term-mode)
     "Term")
    ((or (eq major-mode 'python-mode)
     (eq major-mode 'c-mode)
     (eq major-mode 'c++-mode))
     "Python, C, C++")
    ((or (eq major-mode 'emacs-lisp-mode)
	 (eq major-mode 'org-mode)
	 (eq major-mode 'org-agenda-clockreport-mode)
	 (eq major-mode 'org-src-mode)
	 (eq major-mode 'org-agenda-mode)
	 (eq major-mode 'org-beamer-mode)
	 (eq major-mode 'org-indent-mode)
	 (eq major-mode 'org-bullets-mode)
	 (eq major-mode 'org-cdlatex-mode)
	 (eq major-mode 'org-agenda-log-mode))
     "Emacs lisp and Org mode")
    ((or (eq major-mode 'csv-mode)
	 (eq major-mode 'text-mode))
     "Text and csv")
    ((or (eq major-mode 'sh-mode))
     "Bash")
    (t
     "Misc buffers")
    ))) 

(setq tabbar-ruler-global-tabbar t)  

;; Tabbar ruler
(require 'tabbar-ruler)
;; Hide tabbar in some specific modes
(add-hook 'dashboard-mode-hook 'tabbar-local-mode)
(add-hook 'term-mode-hook 'tabbar-local-mode)
(add-hook 'calendar-mode-hook 'tabbar-local-mode)
(add-hook 'dired-mode-hook 'tabbar-local-mode)
(add-hook 'org-agenda-mode-hook 'tabbar-local-mode)
(add-hook 'magit-log-mode-hook 'tabbar-local-mode)
(add-hook 'magit-diff-mode-hook 'tabbar-local-mode)
(add-hook 'magit-status-mode-hook 'tabbar-local-mode)
(add-hook 'magit-process-mode-hook 'tabbar-local-mode)
(add-hook 'magit-stashes-mode-hook 'tabbar-local-mode)

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

;; Tabbar excluded buffers
(setq tabbar-ruler-excluded-buffers '("*Messages*" "*Completions*" "*ESS*" "*Packages*" "*log-edit-files*" "*helm-mini*" "*helm-mode-describe-variable*" "*scratch*" "*Flycheck error messages*"))

;; Tab change keybinding
(global-set-key (kbd "C-<home>") 'tabbar-press-home)
(global-set-key (kbd "C-<prior>") 'tabbar-backward)
(global-set-key (kbd "C-<next>") 'tabbar-forward)

;; Hide tabbar buttons
(setq tabbar-hide-header-button t)
(setq tabbar-use-images nil)
(defsubst tabbar-line-buttons (tabset)
  "Return a list of propertized strings for tab bar buttons.
TABSET is the tab set used to choose the appropriate buttons."
(list (propertize "")))

;; Which key
(use-package which-key
  :ensure t
  :defer 1
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;; Swiper
(use-package swiper
  :ensure t
  :defer t)
;; Counsel
(use-package counsel
  :ensure t
  :defer t)
;; Ivy configuration
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :bind (("\C-s" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("<f6>" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("<f1> f" . counsel-describe-function)
	 ("<f1> v" . counsel-describe-variable)
	 ("<f1> l" . counsel-find-library)
	 ("<f2> i" . counsel-info-lookup-symbol)
	 ("<f2> u" . counsel-unicode-char)
	 ("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep)
	 ("C-c k" . counsel-ag)
	 ("C-x l" . counsel-locate)
	 ("C-S-o" . counsel-rhythmbox))
  :config
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (setq swiper-goto-start-of-match t))

;; Dashboard
(use-package dashboard
  :ensure t
  :after evil
  :diminish (dashboard-mode page-break-lines-mode)
  :defines (persp-save-dir persp-special-last-buffer)
  :functions (all-the-icons-faicon
	      all-the-icons-material
	      open-custom-file
	      persp-get-buffer-or-null
	      persp-load-state-from-file
	      persp-switch-to-buffer
	      winner-undo
	      widget-forward)
  ;; :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  :hook (dashboard-mode . (lambda ()
			    (setq-local frame-title-format "")
			    (setq-local tab-width 1)))
  :init (dashboard-setup-startup-hook)
  :config
  ;; Configure init info
  (setq dashboard-set-init-info t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;; Disable Evil mode on dashboard
  (add-to-list 'evil-emacs-state-modes 'dashboard-mode)
  ;; Set the title
  (setq dashboard-banner-logo-title "Welcome to Emmacs, boi")
  ;; (setq dashboard-startup-banner "~/.emacs.d/banners/Emmacs2.png")
  (setq dashboard-startup-banner "~/.emacs.d/banners/logo.png")
  (setq dashboard-center-content t)
  (setq dashboard-items '(
			  (recents  . 10)
			  (bookmarks . 5)
			  (agenda . 5)
			  ;; (projects . 0)
			  ;; (registers . 1)
			  ))
  ;; (dashboard-modify-heading-icons '((recents . "alert")
  ;; 				    (bookmarks . "bug")))
  )

;; Smex configuration
(use-package smex
  :ensure t
  :init
  (smex-initialize)) 

(provide '2_emmacs_ui)
;;; 2_emmacs-ui.el ends here
