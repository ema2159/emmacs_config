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
;; - Ivy Rich
;; - Page Break Lines
;; - Pretty Mode
;; - Solaire Mode
;; - Swiper
;; - Treemacs
;; - Which key

;;; Code:
;; All The Icons
(use-package all-the-icons)

;; All The Icons Ivy
(use-package all-the-icons-ivy
  :init
  (setq all-the-icons-ivy-file-commands
	'(counsel-find-file
	  counsel-file-jump
	  counsel-recentf
	  projectile-completing-read))

  (setq all-the-icons-spacer " ")
  :custom-face
  (all-the-icons-ivy-dir-face ((t (:inherit (font-lock-doc-face bold))))))

;; Solaire mode
(use-package solaire-mode
  :config
  (solaire-mode-swap-bg)
  (setq solaire-mode-remap-fringe t)
  (solaire-global-mode +1))

;; Dired
(use-package dired
  :straight nil
  :after (evil hydra)
  :defer t
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
  :after dired
  :bind (:map dired-mode-map
	      ("<return>" . dired-single-buffer)
	      ("<mouse 1>" . 'dired-single-buffer-mouse)))

;; All The Icons Dired
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Dired Hacks
(use-package dired-hacks-utils
  :after dired)

;; Dired Filter
(use-package dired-filter
  :after dired
  :config
  (define-key dired-mode-map (kbd "C-f") dired-filter-map))

;; Dired Subtree
(use-package dired-subtree
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

;; Treemacs
(use-package treemacs
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
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-magit
  :after treemacs magit)

(use-package page-break-lines
  :hook
  (help-mode . page-break-lines-mode))

;; Dashboard
(use-package dashboard
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
  (setq dashboard-startup-banner "~/.emacs.d/banners/escherlogo.png")
  (setq dashboard-center-content t)
  (setq dashboard-items '(
			  (recents  . 8)
			  (bookmarks . 5)
			  ;; (agenda . 5)
			  (projects . 3)
			  (registers . 1))))

(use-package doom-modeline
  :defer 2
  :config
  (column-number-mode)
  (size-indication-mode)
  (setq doom-modeline-buffer-file-name-style 'auto
	doom-modeline-height 30
	doom-modeline-major-mode-color-icon nil
	doom-modeline-lsp t
	doom-modeline-env-version t
	)
  :init
  (doom-modeline-mode)
  :hook
  (after-init . doom-modeline-mode))

;; Centaur Tabs
(use-package powerline)
(when (display-graphic-p)
  (use-package centaur-tabs
    :config
    (setq centaur-tabs-style "bar"
	  centaur-tabs-height 32
	  centaur-tabs-set-icons t
	  centaur-tabs-set-modified-marker t
	  centaur-tabs-show-navigation-buttons t
	  centaur-tabs-set-bar 'under
	  ;; centaur-tabs-gray-out-icons 'buffer
	  ;; centaur-tabs-plain-icons t
	  x-underline-at-descent-line t
	  centaur-tabs-left-edge-margin nil)
    (centaur-tabs-change-fonts (face-attribute 'default :font) 110)
    (centaur-tabs-headline-match)
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
  )

;; Which key
(use-package which-key
  :defer 1
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;; Helpful
(use-package helpful
  :config
  (add-to-list 'evil-emacs-state-modes 'helpful-mode)
  :bind
  (("C-h k" . helpful-key)))

;; Swiper
(use-package swiper
  :defer t
  :after evil
  :bind (:map evil-normal-state-map
	      ("/" . swiper)
	      ("?" . swiper-backward)
	      ("#" . swiper-all-thing-at-point)
	      ("*" . swiper-all-thing-at-point)))

;; Counsel
(use-package counsel
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
	  (set-face-attribute 'highlight-numbers-number nil :weight 'normal)
	  (solaire-mode-swap-bg)
	  (setq centaur-tabs-active-bar
		(centaur-tabs--make-xpm 'centaur-tabs-active-bar-face
					2
					centaur-tabs-bar-height))
	  (centaur-tabs-init-tabsets-store)
	  (centaur-tabs-display-update)
	  (centaur-tabs-headline-match)
	  (set-face-attribute 'default nil :height 130)
	  (centaur-tabs-change-fonts (face-attribute 'default :font) 110))
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
	      ))
  :bind
  (("\C-x\ \C-r" . counsel-recentf)
   ("C-h b" . counsel-descbinds)
   ("C-h a" . counsel-apropos)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h i" . counsel-imenu)
   ("C-x C-p" . counsel-yank-pop)
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

;; Ivy
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq swiper-goto-start-of-match t)
  :bind
  (("\C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("C-x b" . ivy-switch-buffer)))

;; Ivy Posframe
(use-package ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist
	'((swiper          . nil)
	  (counsel-M-x                . ivy-posframe-display-at-frame-top-center)
	  (t                          . nil)))
  (setq ivy-posframe-width 70)
  (ivy-posframe-mode 1))

;; Ivy Rich
(use-package ivy-rich
  :after all-the-icons-ivy
  :preface
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
	(get-buffer candidate)
      (all-the-icons-icon-for-mode major-mode)))
  :init
  (setq ivy-rich-display-transformers-list ; max column width sum = (ivy-poframe-width - 1)
	'(ivy-switch-buffer
	  (:columns
	   ((ivy-rich-switch-buffer-icon (:width 2))
	    (ivy-rich-candidate (:width 35))
	    (ivy-rich-switch-buffer-project (:width 15 :face success))
	    (ivy-rich-switch-buffer-major-mode (:width 13 :face warning)))
	   :predicate
	   #'(lambda (cand) (get-buffer cand)))
	  counsel-M-x
	  (:columns
	   ((counsel-M-x-transformer (:width 35))
	    (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
	  counsel-describe-function
	  (:columns
	   ((counsel-describe-function-transformer (:width 35))
	    (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
	  counsel-describe-variable
	  (:columns
	   ((counsel-describe-variable-transformer (:width 35))
	    (ivy-rich-counsel-variable-docstring (:width 34 :face font-lock-doc-face))))
	  package-install
	  (:columns
	   ((ivy-rich-candidate (:width 25))
	    (ivy-rich-package-version (:width 12 :face font-lock-comment-face))
	    (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
	    (ivy-rich-package-install-summary (:width 23 :face font-lock-doc-face))))))
  :config
  (ivy-rich-mode +1)
  (all-the-icons-ivy-setup)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; AMX
(use-package amx
  :defer t)

;; Pretty mode
(use-package pretty-mode
  :defer t)

;; Custom syntax highlighting
;; Highlight numbers mode
(use-package highlight-numbers
  :config
  (set-face-attribute 'highlight-numbers-number nil :weight 'normal)
  :hook
  (prog-mode . highlight-numbers-mode))

;; Create a font lock face for punctuation signs
(defvar font-lock-punctuation-face 'font-lock-punctuation-face)
(defface font-lock-punctuation-face
  '((t :inherit font-lock-keyword-face))
  "Face for highlighting punctuation signs."
  :group 'emmacs)
(set-face-attribute 'font-lock-punctuation-face nil :weight 'normal)

;; (defvar highlight-numbers-number 'highlight-numbers-number)
;; (defface highlight-numbers-number
;;   '((t :inherit font-lock-constant-face))
;;   "Face for highlighting punctuation signs."
;;   :group 'emmacs)

;; Improve python-mode syntax highlighting
(font-lock-add-keywords 'python-mode
			'(("[\.\,\;\:\*\|\&\!\(\)\{\}\=\$\<\>\'\#\%\-\+]\\|\\]\\|\\[" . font-lock-punctuation-face)
			  ("\\([A-Za-z][A-Za-z0-9_]*\\)[ \t\n]*\\((.*)\\)"
			   (1 font-lock-function-name-face))))
;; Improve js-mode syntax highlighting
(font-lock-add-keywords 'rjsx-mode
			'(("[\.\,\;\:\*\|\&\!\(\)\{\}\=\$\<\>\'\#\%\-\+]\\|\\]\\|\\[" . font-lock-punctuation-face)))
;; Improve c-mode syntax highlighting
(font-lock-add-keywords 'c-mode
			'(("[\.\,\;\:\*\|\&\!\(\)\{\}\=\$\<\>\'\#\%\-\+\@]\\|\\]\\|\\[" . font-lock-punctuation-face)))
;; Improve verilog-mode syntax highlighting
(font-lock-add-keywords 'verilog-mode
			'(("[\.\,\;\:\*\|\&\!\(\)\{\}\=\$\<\>\'\#\%\-\+\@]\\|\\]\\|\\[" . font-lock-punctuation-face)))

;;; Fira code
;; Install Fira Code Symbols
(defun emmacs-fira-install-fonts (&optional pfx)
  "Helper function to download and install the latests fonts based on OS.
When PFX is non-nil, ignore the prompt and just install.
Taken from all-the-icons.el."
  (interactive "P")
  (when (or pfx (yes-or-no-p "This will download and install fonts, are you sure you want to do this?"))
    (let* ((url-format "https://raw.githubusercontent.com/ema2159/emmacs_config/master/other/fonts/%s")
           (font-dest (cl-case window-system
                        (x  (concat (or (getenv "XDG_DATA_HOME")            ;; Default Linux install directories
                                        (concat (getenv "HOME") "/.local/share"))
                                    "/fonts/"))
                        (mac (concat (getenv "HOME") "/Library/Fonts/" ))
                        (ns (concat (getenv "HOME") "/Library/Fonts/" ))))  ;; Default MacOS install directory
           (known-dest? (stringp font-dest))
           (font-dest (or font-dest (read-directory-name "Font installation directory: " "~/"))))

      (unless (file-directory-p font-dest) (mkdir font-dest t))

      (mapc (lambda (font)
              (url-copy-file (format url-format font) (expand-file-name font font-dest) t))
            '("FiraCodeSymbol-Symbol-Regular.ttf"))
      (when known-dest?
        (message "Fonts downloaded, updating font cache... <fc-cache -f -v> ")
        (shell-command-to-string (format "fc-cache -f -v")))
      (message "Successfully %s `Fira Code Symbol' font to `%s'!"
               (if known-dest? "installed" "downloaded")
               font-dest))))

;; When installed, reload
(when (member "Fira Code Symbol" (font-family-list))
  (defun fira-code-mode--make-alist (list)
    "Generate prettify-symbols alist from LIST."
    (let ((idx -1))
      (mapcar
       (lambda (s)
	 (setq idx (1+ idx))
	 (let* ((code (+ #Xf100 idx))
		(width (string-width s))
		(prefix ())
		(suffix '(?\s (Br . Br)))
		(n 1))
	   (while (< n width)
	     (setq prefix (append prefix '(?\s (Br . Bl))))
	     (setq n (1+ n)))
	   (cons s (append prefix suffix (list (decode-char 'ucs code))))))
       list)))

  (defconst fira-code-mode--ligatures
    '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
      "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
      "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
      "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
      ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
      "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
      "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
      "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
      ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
      "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
      "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
      "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
      "x" ":" "+" "+" "*"))

  (defvar fira-code-mode--old-prettify-alist)

  (defun fira-code-mode--enable ()
    "Enable Fira Code ligatures in current buffer."
    (fira-code-mode--setup)
    (setq-local fira-code-mode--old-prettify-alist prettify-symbols-alist)
    (setq-local prettify-symbols-alist (append (fira-code-mode--make-alist fira-code-mode--ligatures) fira-code-mode--old-prettify-alist))
    (prettify-symbols-mode t))

  (defun fira-code-mode--disable ()
    "Disable Fira Code ligatures in current buffer."
    (setq-local prettify-symbols-alist fira-code-mode--old-prettify-alist)
    (prettify-symbols-mode -1))

  (define-minor-mode fira-code-mode
    "Fira Code ligatures minor mode"
    :lighter " Fira Code Symbol"
    (setq-local prettify-symbols-unprettify-at-point 'right-edge)
    (if fira-code-mode
	(fira-code-mode--enable)
      (fira-code-mode--disable)))

  (defun fira-code-mode--setup ()
    "Setup Fira Code Symbols"
    (set-fontset-font t '(#Xf100 . #Xf16f) "Fira Code Symbol"))

  (add-hook 'prog-mode-hook #'fira-code-mode))

(provide '2_emmacs_ui)
;;; 2_emmacs-ui.el ends here
