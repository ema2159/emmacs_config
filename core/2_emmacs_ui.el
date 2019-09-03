;; 2_emmacs-ui.el --- User Insterface

;;; Commentary:
;; In this section the following packages are loaded:
;; - All the icons
;; - All The Icons Ivy
;; - Amx
;; - Centaur Tabs
;; - Counsel
;; - Dashboard
;; - Dired Hacks
;; - Dired Single
;; - Doom Modeline
;; - Helpful
;; - Ivy
;; - Ivy Post Frame
;; - Page Break Lines
;; - Solaire Mode
;; - Swiper
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
  (all-the-icons-ivy-dir-face ((t (:inherit (font-lock-doc-face bold))))))

;; Solaire mode
(use-package solaire-mode
  :ensure t
  :config
  (solaire-mode-swap-bg)
  (setq solaire-mode-remap-fringe t)
  (solaire-global-mode +1))

;; Dired
(use-package dired
  :defer t
  :after evil
  :config
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  (defhydra hydra-dired (:hint nil :color pink)
    "
――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
_+_ mkdir          │ _A_ find regexp  │ _m_ark           │ _(_ details       │ _C-i_nsert-subdir │ C-x C-q : edit
_C_opy             │ _Q_ repl regexp  │ _U_nmark all     │ _)_ omit-mode     │ _C-r_emove-subdir │ C-c C-c : commit
_D_elete           │ _O_ view other   │ _u_nmark         │ _l_ redisplay     │ _$_ hide-subdir   │ C-c ESC : abort
_R_ename           │ _o_pen other     │ _t_oggle         │ _g_ revert buf    │ _w_ kill-subdir   │
_Y_ rel symlink    │ _M_ chmod        │ _E_xtension mark │ _s_ort            │ _e_ ediff         │
_S_ymlink          │ _G_ chgrp        │ _F_ind marked    │ _?_ toggle hydra  │ _=_ pdiff         │
_z_ compress-file  │ _i_nsert-subtree │ ^ ^              │ ^ ^               │                 │
_Z_ compress       │ _r_emove-subtree │                │                 │                 │
_v_iew             │ ^ ^              │                │                 │                 │
――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
"
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff)         ;; smart diff
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy)        ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer)        ;; read all directories again (refresh)
    ("C-i" dired-maybe-insert-subdir)
    ("C-r" dired-kill-subdir)
    ("i" dired-subtree-insert)
    ("r" dired-subtree-remove)
    ("l" dired-do-redisplay)   ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("C-x k" kill-buffer-and-window :exit t)
    ("q" nil)
    ("<escape>" nil)
    ("?" nil :color blue))
  :hook
  (dired-mode .
	      (lambda ()
		(dired-hide-details-mode 1)))
  :bind (:map dired-mode-map
	      ("?" . hydra-dired/body)
	      ("C-i" . dired-maybe-insert-subdir)
	      ("C-r" . dired-kill-subdir)))

;; Dired Single
(use-package dired-single
  :ensure t
  :after dired
  :bind (:map dired-mode-map
	 ("<return>" . dired-single-buffer)
	 ("<mouse 1>" . 'dired-single-buffer-mouse)))

;; All The Icons
(use-package all-the-icons
  :ensure t)

;; All The Icons Dired
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;; Dired Hacks
(use-package dired-hacks-utils
  :ensure t
  :after dired)

;; Dired Filter
(use-package dired-filter
  :ensure t
  :after dired
  :config
  (define-key dired-mode-map (kbd "C-f") dired-filter-map))

;; Dired Subtree
(use-package dired-subtree
  :ensure t
  :after dired-hacks-utils
  :config
  (setq dired-subtree-use-backgrounds nil)
  (add-hook 'dired-subtree-after-insert-hook 'all-the-icons-dired--display)
  ;; :hook
  ;; (dired-subtree-after-insert . (all-the-icons-dired--display))
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

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

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
	    font-lock-keyword-face
	    ""
	    "")
	   (,(all-the-icons-faicon "linkedin" :height 1.5 :v-adjust 0.0)
	    "LinkedIn"
	    "Go to LinkedIn profile"
	    (lambda (&rest _) (browse-url "https://www.linkedin.com/feed/"))
	    font-lock-keyword-face
	    ""
	    "")
	   (,(all-the-icons-material "save" :height 1.5 :v-adjust -0.2)
	    "Previous session"
	    "Restore previous session"
	    (lambda (&rest _) (desktop-read))
	    font-lock-keyword-face
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
      (set-face-attribute 'mode-line nil :height 105)
      (set-face-attribute 'mode-line-inactive nil :height 105)
      :init
      (doom-modeline-mode)
      :hook
      (after-init . doom-modeline-mode))

;; Centaur Tabs
(use-package centaur-tabs
  :load-path "~/.emacs.d/other/centaur-tabs"
  :config
  (setq centaur-tabs-style "bar"
	centaur-tabs-height 32
	centaur-tabs-set-icons t
	centaur-tabs-set-modified-marker t
	centaur-tabs-set-bar 'left)
  (centaur-tabs-headline-match)
  ;; (setq centaur-tabs-gray-out-icons 'buffer)
  ;; (centaur-tabs-enable-buffer-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
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
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-c t s" . centaur-tabs-counsel-switch-group)
  ("C-c t p" . centaur-tabs-group-by-projectile-project)
  ("C-c t g" . centaur-tabs-group-buffer-groups)
  (:map evil-normal-state-map
	("g t" . centaur-tabs-forward)
	("g T" . centaur-tabs-backward)))

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
  :defer t
  :after evil
  :bind (:map evil-normal-state-map
	      ("/" . swiper)
	      ("?" . swiper-backward)))

;; Counsel
(use-package counsel
  :ensure t
  :defer t
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  ;; (setq counsel-descbinds-function #'helpful-key)
  (defun counsel-load-theme-action (x)
    "Disable current themes and load theme X."
    (condition-case nil
	(progn
	  (mapc #'disable-theme custom-enabled-themes)
	  (load-theme (intern x) t)
	  (when (eq centaur-tabs-set-bar 'over)
	    (set-face-attribute 'centaur-tabs-selected nil :overline (face-background 'centaur-tabs-active-bar-face))
	    (set-face-attribute 'centaur-tabs-selected-modified nil :overline (face-background 'centaur-tabs-active-bar-face))
	    (set-face-attribute 'centaur-tabs-unselected nil :overline nil)
	    (set-face-attribute 'centaur-tabs-unselected-modified nil :overline nil))
	  (solaire-mode-swap-bg)
	  (setq centaur-tabs-active-bar
	    (centaur-tabs--make-xpm 'centaur-tabs-active-bar-face
				    2
				    centaur-tabs-bar-height))
	  (centaur-tabs-init-tabsets-store)
	  ;; (centaur-tabs-display-update)
	  (centaur-tabs-force-update)
	  (centaur-tabs-headline-match))
      (error "Problem loading theme %s" x)))
  (defun counsel-load-theme ()
    "Forward to `load-theme'.
Usable with `ivy-resume', `ivy-next-line-and-call' and
`ivy-previous-line-and-call'."
    (interactive)
    (ivy-read "Load custom theme: "
	      (mapcar 'symbol-name
		      (custom-available-themes))
	      :action #'counsel-load-theme-action
	      :caller 'counsel-load-theme
	      ;; :update-fn (lambda() (counsel-load-theme-action (ivy-state-current ivy-last)))
	      ))
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
   ("C-x C-l" . counsel-load-theme)
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

;; Ivy Posframe configuration
(use-package ivy-posframe
  :ensure t
  :config
  (setq ivy-posframe-display-functions-alist
	'((swiper          . nil)
	  (counsel-M-x                . ivy-posframe-display-at-point)
	  (t                          . nil)))
  (ivy-posframe-mode 1))

;; AMX configuration
(use-package amx
  :ensure t
  :defer t)

(provide '2_emmacs_ui)
;;; 2_emmacs-ui.el ends here
