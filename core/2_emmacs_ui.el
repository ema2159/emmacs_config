;; 2_emmacs-ui.el --- User Insterface

;;; Commentary:
;; In this section the following packages are loaded:
;; - All the icons
;; - All The Icons Ivy
;; - Amx
;; - Counsel
;; - Counsel Projectile
;; - Dashboard
;; - Dired Hacks
;; - Dired Single
;; - Doom Modeline
;; - Helpful
;; - Ivy
;; - Page Break Lines
;; - Solaire Mode
;; - Swiper
;; - Tabbar Ruler
;; - Treemacs
;; - Which key

;;; Code:
;; All The Icons
(use-package all-the-icons
  :ensure t)

;; All The Icons Ivy
(use-package all-the-icons-ivy
  :ensure t
  :after ivy
  :init
  (setq all-the-icons-ivy-file-commands
	'(counsel-find-file
	  counsel-file-jump
	  counsel-recentf
	  projectile-completing-read))
	  
  (setq all-the-icons-spacer " ")
  :config
  (all-the-icons-ivy-setup)
  :custom-face
  (all-the-icons-ivy-dir-face ((t (:inherit (mode-line-emphasis bold))))))

;; Solaire mode
(use-package solaire-mode
  :ensure t
  :config
  (solaire-mode-swap-bg)
  (setq solaire-mode-remap-fringe t)
  (solaire-global-mode +1)
  )

;; Dired
(use-package dired
  :defer t
  :after evil
  :config
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  :hook
  (dired-mode .
	      (lambda ()
		(dired-hide-details-mode 1)))
  :bind (:map dired-mode-map
	      ("C-i" . dired-maybe-insert-subdir)
	      ("C-r" . dired-kill-subdir)))

;; Dired Single
(use-package dired-single
  :ensure t
  :after dired
  :bind (:map dired-mode-map
	 ("<return>" . dired-single-buffer)
	 ("<mouse 1>" . 'dired-single-buffer-mouse)))

;; All The Icons Dired
(if (display-graphic-p)
    (progn
      ;; if graphic
      (use-package all-the-icons)
      (use-package all-the-icons-dired
	:hook (dired-mode . all-the-icons-dired-mode))))

;; Dired Hacks
(use-package dired-hacks-utils
  :ensure t
  :after dired)
(use-package dired-filter
  :ensure t
  :after dired
  :config
  (define-key dired-mode-map (kbd "C-f") dired-filter-map))
(use-package dired-subtree
  :ensure t
  :after dired-hacks-utils
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind
  (:map dired-mode-map
   ("i" . dired-subtree-insert)
   ("r" . dired-subtree-remove)))

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

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;; Telephone line
;; (use-package telephone-line
;;   :ensure t
;;   :config
;;   (setq telephone-line-primary-left-separator 'telephone-line-sin-left
;; 	telephone-line-secondary-left-separator 'telephone-line-sin-hollow-left
;; 	telephone-line-primary-right-separator 'telephone-line-sin-right
;; 	telephone-line-secondary-right-separator 'telephone-line-sin-hollow-right)
;;   (telephone-line-mode 1)
;;   (set-face-attribute 'telephone-line-evil-normal
;; 		      nil
;; 		      :background
;; 		      "#317DC9"))

(use-package page-break-lines
  :ensure t
  :hook
  (help-mode . page-break-lines-mode))

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
  :init (dashboard-setup-startup-hook)
  :config
  ;; Configure init info
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-footer t)
  (setq dashboard-navigator-buttons
	`(((,(all-the-icons-octicon "mark-github" :height 1.5 :v-adjust 0.0)
	    "Github"
	    "Go to Github profile"
	    (lambda (&rest _) (browse-url "https://github.com/ema2159"))
	    'font-lock-keyword-face
	    ""
	    "")
	   (,(all-the-icons-faicon "linkedin" :height 1.5 :v-adjust 0.0)
	    "LinkedIn"
	    "Go to LinkedIn profile"
	    (lambda (&rest _) (browse-url "https://www.linkedin.com/feed/"))
	    'font-lock-keyword-face
	    ""
	    "")
	   (,(all-the-icons-material "save" :height 1.5 :v-adjust -0.2)
	    "Previous session"
	    "Restore previous session"
	    (lambda (&rest _) (desktop-read))
	    'font-lock-keyword-face
	    ""
	    ""))))
  ;; Disable Evil mode on dashboard
  (add-to-list 'evil-emacs-state-modes 'dashboard-mode)
  ;; Set the title
  (setq dashboard-banner-logo-title "Welcome to Emmacs, boi")
  (setq dashboard-startup-banner "~/.emacs.d/banners/logo.png")
  ;; (setq dashboard-startup-banner 3)
  (setq dashboard-center-content t)
  (setq dashboard-items '(
			  (recents  . 8)
			  (bookmarks . 5)
			  (agenda . 5)
			  (projects . 3)
			  (registers . 1))))

(use-package doom-modeline
      :ensure t
      :defer 2
      :config
      (column-number-mode)
      (size-indication-mode)
      (setq doom-modeline-buffer-file-name-style 'file-name)
      (set-face-attribute 'doom-modeline-evil-insert-state
			   		      nil
			   		      :foreground
			   		      "#317DC9")
      :init
      (doom-modeline-mode)
      :hook
      (after-init . doom-modeline-mode))

;; Tabbar ruler
;; (require 'emacs-tabs)
;; (defun tabbar-buffer-groups ()
;;   (list
;;    (cond
;;     ((or (eq major-mode 'magit-log-mode)
;; 	 (eq major-mode 'magit-mode)
;; 	 (eq major-mode 'magit-wip-mode)
;; 	 (eq major-mode 'magit-blob-mode)
;; 	 (eq major-mode 'magit-diff-mode)
;; 	 (eq major-mode 'magit-file-mode)
;; 	 (eq major-mode 'magit-refs-mode)
;; 	 (eq major-mode 'magit-blame-mode)
;; 	 (eq major-mode 'magit-popup-mode)
;; 	 (eq major-mode 'magit-stash-mode)
;; 	 (eq major-mode 'magit-cherry-mode)
;; 	 (eq major-mode 'magit-reflog-mode)
;; 	 (eq major-mode 'magit-status-mode)
;; 	 (eq major-mode 'magit-process-mode)
;; 	 (eq major-mode 'magit-stashes-mode)
;; 	 (eq major-mode 'magit-repolist-mode)
;; 	 (eq major-mode 'magit-revision-mode)
;; 	 (eq major-mode 'magit-log-select-mode)
;; 	 (eq major-mode 'magit-popup-help-mode)
;; 	 (eq major-mode 'magit-auto-revert-mode)
;; 	 (eq major-mode 'magit-merge-preview-mode)
;; 	 (eq major-mode 'magit-submodule-list-mode)
;; 	 (eq major-mode 'magit-wip-after-save-mode)
;; 	 (eq major-mode 'magit-blame-read-only-mode)
;; 	 (eq major-mode 'magit-wip-after-apply-mode)
;; 	 (eq major-mode 'magit-wip-before-change-mode)
;; 	 (eq major-mode 'magit-wip-initial-backup-mode)
;; 	 (eq major-mode 'magit-wip-after-save-mode))
;;      "Magit")
;;     ((eq major-mode 'dired-mode)
;;      "Dired")
;;     ((eq major-mode 'dashboard-mode)
;;      "Dashboard")
;;     ((eq major-mode 'term-mode)
;;      "Term")
;;     ((or (eq major-mode 'helpful-mode)
;; 	 (eq major-mode 'help-mode))
;;      "Help")
;;     ((or (eq major-mode 'org-mode)
;; 	 (eq major-mode 'org-agenda-clockreport-mode)
;; 	 (eq major-mode 'org-src-mode)
;; 	 (eq major-mode 'org-agenda-mode)
;; 	 (eq major-mode 'org-beamer-mode)
;; 	 (eq major-mode 'org-indent-mode)
;; 	 (eq major-mode 'org-bullets-mode)
;; 	 (eq major-mode 'org-cdlatex-mode)
;; 	 (eq major-mode 'org-agenda-log-mode))
;;      "Org mode")
;;     (t
;;      "Editing")
;;     )))

;; ;; Hide tabbar in some specific modes
;; (add-hook 'dashboard-mode-hook 'tabbar-local-mode)
;; (add-hook 'term-mode-hook 'tabbar-local-mode)
;; (add-hook 'calendar-mode-hook 'tabbar-local-mode)
;; (add-hook 'dired-mode-hook 'tabbar-local-mode)
;; (add-hook 'org-agenda-mode-hook 'tabbar-local-mode)
;; (add-hook 'magit-log-mode-hook 'tabbar-local-mode)
;; (add-hook 'magit-diff-mode-hook 'tabbar-local-mode)
;; (add-hook 'magit-status-mode-hook 'tabbar-local-mode)
;; (add-hook 'magit-process-mode-hook 'tabbar-local-mode)
;; (add-hook 'magit-stashes-mode-hook 'tabbar-local-mode)
;; (add-hook 'helpful-mode-hook 'tabbar-local-mode)
;; (add-hook 'help-mode-hook 'tabbar-local-mode)
;; (add-hook 'fundamental-mode-hook 'tabbar-local-mode)

;; (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

;; ;; Tabbar excluded buffers
;; (setq emacs-tabs-excluded-buffers '("*Messages*"
;; 				      "*Completions*"
;; 				      "*ESS*"
;; 				      "*Packages*"
;; 				      "*log-edit-files*"
;; 				      "*helm-mini*"
;; 				      "*helm-mode-describe-variable*"
;; 				      "*scratch*"
;; 				      "*Flycheck error messages*"
;; 				      "*Compile-Log*"
;; 				      "*tramp*"
;; 				      "*Help*"
;; 				      "*company-documentation*"
;; 				      "*Flymake log*"
;; 				      "*anaconda-mode*"
;; 				      "*Anaconda*"))

;; ;; Tab change keybinding
;; (global-set-key (kbd "C-<home>") 'tabbar-press-home)
;; (global-set-key (kbd "C-<prior>") 'tabbar-backward)
;; (global-set-key (kbd "C-<next>") 'tabbar-forward)

;; ;; Hide tabbar buttons
;; (setq tabbar-hide-header-button t)
;; (setq tabbar-use-images nil)
;; (defsubst tabbar-line-buttons (tabset)
;;   "Return a list of propertized strings for tab bar buttons.
;; TABSET is the tab set used to choose the appropriate buttons."
;; (list (propertize "")))

(use-package centaur-tabs
  :load-path "~/.emacs.d/other/centaur-tabs"
  :config
  (setq centaur-tabs-background-color (face-background 'default))
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-height 30)
  (setq centaur-tabs-set-icons nil)
  (centaur-tabs-inherit-tabbar-faces)
  (centaur-tabs-mode t)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
	   (memq major-mode '(magit-process-mode
			      magit-status-mode
			      magit-diff-mode
			      magit-log-mode
			      magit-file-mode
			      magit-blob-mode
			      magit-blame-mode
			      )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
			  help-mode))
       "Help")
      ((memq major-mode '(org-mode
			  org-agenda-clockreport-mode
			  org-src-mode
			  org-agenda-mode
			  org-beamer-mode
			  org-indent-mode
			  org-bullets-mode
			  org-cdlatex-mode
			  org-agenda-log-mode
			  diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  ;; (defun centaur-tabs-buffer-groups ()
  ;;   (list
  ;;    (cond
  ;;     ((memq major-mode '(magit-log-mode
  ;; 			  magit-mode
  ;; 			  magit-wip-mode
  ;; 			  magit-blob-mode
  ;; 			  magit-diff-mode
  ;; 			  magit-file-mode
  ;; 			  magit-refs-mode
  ;; 			  magit-blame-mode
  ;; 			  magit-popup-mode
  ;; 			  magit-stash-mode
  ;; 			  magit-cherry-mode
  ;; 			  magit-reflog-mode
  ;; 			  magit-status-mode
  ;; 			  magit-process-mode
  ;; 			  magit-stashes-mode
  ;; 			  magit-repolist-mode
  ;; 			  magit-revision-mode
  ;; 			  magit-log-select-mode
  ;; 			  magit-popup-help-mode
  ;; 			  magit-auto-revert-mode
  ;; 			  magit-merge-preview-mode
  ;; 			  magit-submodule-list-mode
  ;; 			  magit-wip-after-save-mode
  ;; 			  magit-blame-read-only-mode
  ;; 			  magit-wip-after-apply-mode
  ;; 			  magit-wip-before-change-mode
  ;; 			  magit-wip-initial-backup-mode
  ;; 			  magit-wip-after-save-mode))
  ;;      "Magit")
  ;;     ((derived-mode-p 'dired-mode)
  ;;      "Dired")
  ;;     ((derived-mode-p 'dashboard-mode)
  ;;      "Dashboard")
  ;;     ((derived-mode-p 'term-mode)
  ;;      "Term")
  ;;     ((memq major-mode '(helpful-mode
  ;; 			  help-mode))
  ;;      "Help")
  ;;     ((memq major-mode '(org-mode
  ;; 			  org-agenda-clockreport-mode
  ;; 			  org-src-mode
  ;; 			  org-agenda-mode
  ;; 			  org-beamer-mode
  ;; 			  org-indent-mode
  ;; 			  org-bullets-mode
  ;; 			  org-cdlatex-mode
  ;; 			  org-agenda-log-mode))
  ;;      "Org mode")
  ;;     ((derived-mode-p 'prog-mode)
  ;;      "Editing")
  ;;     (t
  ;;      (awesome-tab-get-group-name (current-buffer))))))
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (dired-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (magit-log-mode . centaur-tabs-local-mode)
  (magit-diff-mode . centaur-tabs-local-mode)
  (magit-status-mode . centaur-tabs-local-mode)
  (magit-process-mode . centaur-tabs-local-mode)
  (magit-stashes-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  (help-mode . centaur-tabs-local-mode)
  (fundamental-mode . centaur-tabs-local-mode)
  (lisp-interaction-mode . centaur-tabs-local-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
   ("C-<next>" . centaur-tabs-forward))

;; Which key
(use-package which-key
  :ensure t
  :defer 1
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;; Helpful
(use-package helpful
  :ensure t
  :config
  (add-to-list 'evil-emacs-state-modes 'helpful-mode)
  :bind
  (("C-h k" . helpful-key)))

;; Swiper
(use-package swiper
  :ensure t
  :defer t)

;; Counsel
(use-package counsel
  :ensure t
  :defer t
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  :bind
  (("\C-x\ \C-r" . counsel-recentf)
   ("C-h b" . counsel-descbinds)
   ("C-h a" . counsel-apropos)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h i" . counsel-imenu)
   ("C-p" . counsel-yank-pop)
   ("C-x r b" . counsel-bookmark)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c u" . counsel-unicode-char)
   (:map
    minibuffer-local-map
    ("C-r" . counsel-minibuffer-history))))

;; Ivy configuration
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :bind
  (("\C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("C-x b" . ivy-switch-buffer))
  :config
  (setq swiper-goto-start-of-match t))

;; AMX configuration
(use-package amx
  :ensure t
  :defer t) 

(provide '2_emmacs_ui)
;;; 2_emmacs-ui.el ends here
