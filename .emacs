 
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Package repos
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize) 


;; GENERAL CONFIGURATION
;; When split, if a buffer is killed, also its window is killed
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)
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


;; Solaire mode
(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

(require 'spacemacs-dark-theme)
;; (require 'doom-one-theme)
;; (require 'solarized-dark-theme)
;; (require 'doom-city-lights-theme)
;; (require 'doom-dracula-theme)
;; (require 'atom-one-dark-theme)

;; Theme
(if (display-graphic-p)
    (progn
    ;; if graphic
      (load-theme 'spacemacs-dark t))
      ;; (load-theme 'solarized-dark t))
      ;; (load-theme 'doom-one t))
      ;; (load-theme 'doom-city-lights t))
      ;; (load-theme 'doom-dracula t))
      ;; (load-theme 'atom-one-dark t))
    ;; else (optional)
  (load-theme 'atom-one-dark t))

(solaire-mode-swap-bg)
(doom-themes-org-config)

;; GLOBAL VARIABLES
;; Define a variable for hooks to turn on/off the relative and absolute number lines
(defvar linum-active t)

;; Function for saving and killing buffer and window
(defun save-and-kill-buffer-and-window ()
  (interactive)
  (save-buffer)
  (kill-buffer-and-window)
  )

;; Evil mode for VIM key bindings
(use-package evil
  :ensure t
  :init
  (evil-mode)
  :config
  ;; When evil :q[uit], close buffer and window instead of Emacs
  (evil-ex-define-cmd "q[uit]" 'kill-buffer-and-window)
  ;; When evil :wq, save and close buffer and window instead of Emacs
  (evil-ex-define-cmd "wq" 'save-and-kill-buffer-and-window))


;; Drag stuff
(use-package drag-stuff
  :ensure
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

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
(require 'dired-single)
(defun my-dired-init ()
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse) 
  (define-key dired-mode-map [tab] 'ranger-mode) 
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer "..")))))
;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

;; Load all the icons for Dired
(add-to-list 'load-path "~/.emacs.d/elpa/font-lock-plus")
(load "font-lock+.el")
(require 'font-lock)
;; Improve python-mode syntax highlighting
(font-lock-add-keywords 'python-mode
  '(("-?\\b[0-9]+\\.?" . font-lock-constant-face)
    ("[\.\,\;\:\*\|\&\!\(\)\{\}\=\$\<\>\'\#\%\-\+\@]\\|\\]\\|\\[" . font-lock-keyword-face)
    ("\\([A-Za-z][A-Za-z0-9_]*\\)[ \t\n]*\\((.*)\\)"
     (1 font-lock-function-name-face))))
(require 'font-lock+)
(font-lock-mode 0)
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "565aa482e486e2bdb9c3cf5bfb14d1a07c4a42cfc0dc9d6a14069e53b6435b56" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "bf5bdab33a008333648512df0d2b9d9710bdfba12f6a768c7d2c438e1092b633" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(fci-rule-color "#62686E" t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f24" "#51afef") t)
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f24" "#7bc275") t)
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f24" "#484854") t)
 '(package-selected-packages
   (quote
    (dumb-jump beacon smartparens avy highlight-indent-guides dash dired-hacks-utils drag-stuff solarized-theme markdown-preview-eww planet-theme material-theme smex sublimity org-bullets org-evil org load-theme-buffer-local color-theme-buffer-local shell-pop ranger all-the-icons-dired dired-single evil-multiedit multiple-cursors page-break-lines dashboard yasnippet-snippets company-jedi ein csv-mode ivy-yasnippet counsel ivy flycheck company which-key telephone-line ## magit projectile use-package treemacs-evil treemacs tabbar-ruler tabbar linum-relative nlinum atom-one-dark-theme spacemacs-theme klere-theme evil color-theme)))
 '(shell-pop-default-directory "/Users/kyagi/git")
 '(shell-pop-full-span t)
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(shell-pop-term-shell "/bin/bash")
 '(shell-pop-universal-key "<f5>")
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 30)
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]] t)
 '(tramp-ssh-controlmaster-options
   "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p -o ControlMaster=auto -o ControlPersist=yes" t)
 '(vc-annotate-background "#242730" t)
 '(vc-annotate-color-map
   (list
    (cons 20 "#7bc275")
    (cons 40 "#a6c677")
    (cons 60 "#d1ca79")
    (cons 80 "#FCCE7B")
    (cons 100 "#f4b96e")
    (cons 120 "#eda461")
    (cons 140 "#e69055")
    (cons 160 "#db8981")
    (cons 180 "#d082ae")
    (cons 200 "#C57BDB")
    (cons 220 "#d874b0")
    (cons 240 "#eb6d86")
    (cons 260 "#ff665c")
    (cons 280 "#d15e59")
    (cons 300 "#a35758")
    (cons 320 "#754f56")
    (cons 340 "#62686E")
    (cons 360 "#62686E")) t)
 '(vc-annotate-very-old-color nil t))


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
(add-to-list 'load-path "~/.emacs.d/elpa/specman")
(load "specman-mode")
(put 'specman-mode 'derived-mode-parent 'prog-mode)
(add-to-list 'auto-mode-alist '("\\.e\\'" . specman-mode))
(add-to-list 'auto-mode-alist '("\\.ecom\\'" . specman-mode))
(add-hook 'specman-mode-hook (lambda () (use-local-map nil)))
(add-hook 'specman-mode-hook 'yas-minor-mode)


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

;; Recent files
(use-package recentf
  :ensure t
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (global-set-key "\C-x\ \C-r" 'recentf-open-files))

;; Which key
(use-package which-key
  :defer 5
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  :init
  (add-hook 'which-key-init-buffer-hook (lambda () (setq-local linum-active nil))))

;; Company
(use-package company
  :ensure t
  :defer 5
  :init
  (company-mode 1)
  (add-hook 'after-init-hook 'global-company-mode))

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
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

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
;;       '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;; 	("http" . "web-proxy.rose.hpecorp.net:8088")
;; 	("https" . "web-proxy.rose.hpecorp.net:8088")))

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
(setq tramp-verbose 1)


;; Csv-mode configuration
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; EIN configuration
;; (require 'ein)
;; (require 'ein-notebook)
;; (require 'ein-subpackages)

;; Evil multiedit
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

;; Configure backup files
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Tabbar ruler
(require 'tabbar-ruler)
(setq dashboard-center-content t)
;; Hide tabbar in some specific modes
(add-hook 'dashboard-mode-hook 'tabbar-local-mode)
(add-hook 'ivy-mode-hook 'tabbar-local-mode)
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
    (t
     "Misc buffers")
    ))) 

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

;; Tabbar excluded buffers
(setq tabbar-ruler-excluded-buffers '("*Messages*" "*Completions*" "*ESS*" "*Packages*" "*log-edit-files*" "*helm-mini*" "*helm-mode-describe-variable*" "*scratch*"))


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

;; Dashboard
(require 'page-break-lines)
(require 'dashboard)
(dashboard-setup-startup-hook)
;; Disable Evil mode on dashboard
(add-to-list 'evil-emacs-state-modes 'dashboard-mode)
;; Set the title
(setq dashboard-banner-logo-title "Welcome to Emmacs, boi")
(setq dashboard-startup-banner "~/.emacs.d/banners/Emmacs.png")
(setq dashboard-items '((recents  . 10)
			(agenda . 5)
                        (bookmarks . 5)))

;; Magit
;; (require 'magit)
(use-package magit
  :bind ("C-x g" . magit-status)
  :init
  (add-hook 'magit-process-mode-hook
	    (lambda () (setq-local linum-active nil))))

;; Shell-pop
(require 'shell-pop)
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
(setq org-support-shift-select 'always)
;; Org bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
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

;; Markdown preview
(require 'markdown-preview-eww)
(add-hook 'eww-mode-hook
	  (lambda () (setq-local linum-active nil)))
(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'eww-mode-hook 'prefer-horizontal-split)

;; Highlight indent guides
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

;; Avy
(use-package avy
  :bind* ("C-'" . avy-goto-word-1)
  :config
  (avy-setup-default)
  (setq avy-background t))

;; Smart parens
(require 'smartparens-config)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'c-mode-hook #'smartparens-mode)
(add-hook 'c++-mode-hook #'smartparens-mode)
(add-hook 'specman-mode-hook #'smartparens-mode)

;; Beacon
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

;; Dumb jump
(dumb-jump-mode)
;; Enable dumb jump on evil mode
(eval-after-load 'evil-maps
  '(progn
     (define-key evil-motion-state-map "gd" 'dumb-jump-go)))


;; ESS
(use-package ess-r-mode)

;; Windmove
(use-package windmove
  :bind
  ("C-x C-<left>"  . windmove-left)
  ("C-x C-<right>" . windmove-right)
  ("C-x C-<up>"    . windmove-up)
  ("C-x C-<down>"  . windmove-down))
