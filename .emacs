 
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; GC threshold to 1GB
(setq gc-cons-threshold 1000000000
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold 800000
		  gc-cons-percentage 0.1)))

(package-initialize)

;; Bootstrap 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-compute-statistics t)

;; Package repos
(require 'package)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

(setq package-enable-at-startup nil)

(defvar emacs-dir (expand-file-name user-emacs-directory)
  "The path to this emacs.d directory.")

(defvar core-dir  "core/"
  "Where essential files are stored.")

;; Configuration loading
(use-package el-init)

(el-init-load emacs-dir
  :subdirectories (list core-dir))

;; Elpy
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

;; Configure Dired
(require 'dired)
(defun xah-dired-mode-setup ()
  (dired-hide-details-mode 1))
;; Make emacs mode dired default state
(add-to-list 'evil-emacs-state-modes 'dired-mode)
;; Make Dired less verbose
(add-hook 'dired-mode-hook
	  (lambda () (setq-local linum-active nil)))
(add-hook 'dired-mode-hook 'xah-dired-mode-setup)

;; Dired Single
(use-package dired-single
  :ensure t)
(defun my-dired-init ()
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse) 
  ;; (define-key dired-mode-map [tab] 'ranger-mode) 
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer "..")))))
;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

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
;; Improve c-mode syntax highlighting
(font-lock-add-keywords 'c-mode
  '(("-?\\b[0-9]+\\.?" . font-lock-constant-face)
    ("[\.\,\;\:\*\|\&\!\(\)\{\}\=\$\<\>\'\#\%\-\+\@]\\|\\]\\|\\[" . font-lock-punctuation-face)))
(if (display-graphic-p)
    (progn
      ;; if graphic
      (use-package all-the-icons)
      (use-package all-the-icons-dired
	:hook (dired-mode . treemacs-icons-dired-mode)))
  ;; else (optional)
  )

;; Dired Hacks
(use-package dired-hacks-utils
  :ensure t)
(use-package dired-filter
  :ensure t
  :config
  (define-key dired-mode-map (kbd "C-f") dired-filter-mark-map))
(use-package dired-subtree
  :ensure t
  :config
  (setq dired-subtree-use-backgrounds nil)
  (define-key dired-mode-map (kbd "i") 'dired-subtree-insert)
  (define-key dired-mode-map (kbd "r") 'dired-subtree-remove))

;; Line numbers configuration
;; Toggle between relative and absolute line numbers between evil normal and insert mode
(require 'linum)
(use-package linum-relative
  :ensure t
  :config
  ;; Remove underline (for paren-mode bug)
  (set-face-attribute 'linum-relative-current-face nil :underline nil)
  (defun normal-linum ()
    "Activates relative number lines"
    (linum-relative-on))
  (add-hook 'evil-normal-state-entry-hook
	    (lambda () (when linum-active (normal-linum))))
  (defun insert-linum ()
    "Activates absolute number lines"
    (linum-relative-off)
    (linum-on))
  (add-hook 'evil-insert-state-entry-hook
	    (lambda () (when linum-active (insert-linum)))))


;; Set specman mode
(add-to-list 'load-path "~/.emacs.d/specman")
(load "specman-mode")
(put 'specman-mode 'derived-mode-parent 'prog-mode)
(add-to-list 'auto-mode-alist '("\\.e\\'" . specman-mode))
(add-to-list 'auto-mode-alist '("\\.ecom\\'" . specman-mode))
(add-hook 'specman-mode-hook (lambda () (use-local-map nil)))
(add-hook 'specman-mode-hook 'yas-minor-mode)

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


(use-package mode-icons
  :ensure t)

(require 'mode-icons)
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

;; Tabbar groups
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

;; Tabbar ruler
;; (use-package tabbar-ruler
;;   :ensure t
;;   :init
;;   :config
;;   ;; Hide tabbar in some specific modes
;;   (add-hook 'dashboard-mode-hook 'tabbar-local-mode)
;;   (add-hook 'term-mode-hook 'tabbar-local-mode)
;;   (add-hook 'calendar-mode-hook 'tabbar-local-mode)
;;   (add-hook 'dired-mode-hook 'tabbar-local-mode)
;;   (add-hook 'org-agenda-mode-hook 'tabbar-local-mode)
;;   (add-hook 'magit-log-mode-hook 'tabbar-local-mode)
;;   (add-hook 'magit-diff-mode-hook 'tabbar-local-mode)
;;   (add-hook 'magit-status-mode-hook 'tabbar-local-mode)
;;   (add-hook 'magit-process-mode-hook 'tabbar-local-mode)
;;   (add-hook 'magit-stashes-mode-hook 'tabbar-local-mode)

;;   (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
;;   ;; Tabbar excluded buffers
;;   (setq tabbar-ruler-excluded-buffers '("*Messages*" "*Completions*" "*ESS*" "*Packages*" "*log-edit-files*" "*helm-mini*" "*helm-mode-describe-variable*" "*scratch*" "*Flycheck error messages*"))
;;   ;; Hide tabbar buttons
;;   (setq tabbar-hide-header-button t)
;;   (setq tabbar-use-images nil)
;;   (defsubst tabbar-line-buttons (tabset)
;;     "Return a list of propertized strings for tab bar buttons.
;; TABSET is the tab set used to choose the appropriate buttons."
;;     (list (propertize "")))
;;   :bind (("C-<home>" . tabbar-press-home)
;; 	 ("C-<prior>" . tabbar-backward)
;; 	 ("C-<next>" . tabbar-forward))
;;   )
;; (setq tabbar-ruler-global-tabbar t)  

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

;; Tabbar groups
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

;; Recent files
(use-package recentf
  :ensure t
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (global-set-key "\C-x\ \C-r" 'recentf-open-files))

;; Which key
(use-package which-key
  :ensure t
  :defer 5
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  :init
  (add-hook 'which-key-init-buffer-hook
	    (lambda () (setq-local linum-active nil))))

;; Flycheck
(use-package flycheck
  :ensure t
  :hook
  (
   (flycheck-error-list-mode . (lambda () (setq-local linum-active nil))))
  :init
  (global-flycheck-mode))


;; Irnony
(use-package irony
  :ensure t
  :hook
  ((c-mode . irony-mode)
   (c++-mode . irony-mode)
   (irony-mode . irony-cdb-autosetup-compile-options)))

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

;; Company box
;; (use-package company-box
;;   :hook
;;   (company-mode . company-box-mode)
;;   :config
;;   (company-box-mode . (lambda () (setq-local linum-active nil))))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"                 ;; personal snippets
	 ;; "~/.emacs.d/elpa/yasnippet-snippets-.*/snippets" ;; the yasmate collection
	  ))
  ;; Add yasnippet support for all company backends
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
	      '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  (yas-global-mode 1)
  (setq require-final-newline nil)

  ;; Yas auxiliar functions
  (defun if-yas-empty ()
    (interactive)
    (if (and yas-moving-away-p (not yas-modified-p))
	(yas-clear-field))))

;; Swiper
(use-package swiper
  :ensure t)
;; Counsel
(use-package counsel
  :ensure t)
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

;; Ivy yasnippet
(use-package ivy-yasnippet
  :ensure t
  :bind
  ("C-x C-a" . ivy-yasnippet))

;; Smex configuration
(use-package smex
  :ensure t
  :init
  (smex-initialize)) 

;; Set proxy
;; (setq url-proxy-services
;;        '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;  	("http" . "web-proxy.rose.hpecorp.net:8088")
;;  	("https" . "web-proxy.rose.hpecorp.net:8088")))

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


;; Csv-mode configuration
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; EIN configuration
;; (require 'ein)
;; (require 'ein-notebook)
;; (require 'ein-subpackages)

;; Configure backup files
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Dashboard
(use-package dashboard
  :ensure t
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

;; Magit
;; (require 'magit)
(use-package magit
  :bind ("C-x g" . magit-status)
  :init
  (add-hook 'magit-process-mode-hook
	    (lambda () (setq-local linum-active nil))))

;; Shell-pop
(use-package shell-pop
  :ensure t)
;; Change theme in term mode
(add-hook 'term-mode-hook (lambda() (load-theme-buffer-local 'cyberpunk (current-buffer))))
;; Disable evil mode in term mode
(evil-set-initial-state 'term-mode 'emacs)
;; Term background and foreground faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term ((t (:background "black" :foreground "white")))))
;; Turn off telephone line in term-mode
;; (add-hook 'term-mode-hook
;;	  (lambda () (setq-local mode-line-format nil)))

;; Org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-agenda-files (list "~/org/week.org"))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-support-shift-select t)
;; Org bullets
(use-package org-bullets
  :ensure t
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))
;; Turn off line numbers in org mode
(add-hook 'org-mode-hook
	  (lambda () (setq-local linum-active nil)))
;; Org mouse for checkboxes
(require 'org-mouse)
;; Hide markers for italic, bold etc
(setq org-hide-emphasis-markers t)
;; Print timestamp when done
(setq org-log-done 'time)
;; Org export backends
(require 'ox-md)
;; Handle source blocks
(setq org-src-tab-acts-natively t)
;; Enable word wrap
(add-hook 'org-mode-hook #'toggle-word-wrap)
(add-hook 'org-mode-hook #'toggle-truncate-lines)
;; Make LaTex use minted when exporting
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"))


;; Highlight indent guides
(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'stack)
  :hook
  (prog-mode-hook . highlight-indent-guides-mode))

;; Avy
(use-package avy
  :bind* ("C-'" . avy-goto-word-1)
  :config
  (avy-setup-default)
  (setq avy-background t))

;; Smart parens
(use-package smartparens
  :ensure t
  :hook
  (python-mode . smartparens-mode)
  (emacs-lisp-mode . smartparens-mode)
  (c-mode . smartparens-mode)
  (c++-mode . smartparens-mode)
  (specman-mode . smartparens-mode))

;; Beacon
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

;; ESS
;; (use-package ess
;;   :ensure t)

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

;; Column enforce mode
(use-package column-enforce-mode
  :ensure t
  :hook
  (prog-mode . column-enforce-mode))

;; Expand Region
(use-package expand-region
  :ensure t
  :bind
  ("C-0"  . 'er/expand-region))

;; Highlight thing
(use-package highlight-thing
  :ensure t
  :config
  (set-face-attribute 'highlight-thing nil :inherit 'highlight :background "#494949")
  (add-hook 'iedit-mode-hook (lambda()
			       (highlight-thing-mode -1)))

  (add-hook 'iedit-mode-end-hook (lambda()
				   (highlight-thing-mode 1)))
  :init
  (setq highlight-thing-what-thing 'symbol)
  (setq highlight-thing-delay-seconds 0.5)
  (setq highlight-thing-all-visible-buffers-p t)
  :hook
  (prog-mode . highlight-thing-mode)
  (specman-mode . highlight-thing-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (highlight-thing expand-region column-enforce-mode dumb-jump smartparens-config which-key use-package treemacs-magit treemacs-icons-dired treemacs-evil telephone-line tabbar-ruler solarized-theme solaire-mode smex shell-pop org-bullets linum-relative ivy-yasnippet irony flycheck evil-multiedit elpy drag-stuff doom-themes dired-subtree dired-single dired-filter dashboard counsel beacon))))
