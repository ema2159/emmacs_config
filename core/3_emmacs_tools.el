;;; 3_emmacs_tools.el --- Tools

;;; Commentary:
;; In this section the following packages are loaded:
;; - Avy
;; - Beacon
;; - Column Enforce Mode
;; - Company
;; - Company Quick Help
;; - Drag Stuff
;; - Dumb Jump
;; - Evil
;; - Evil Easymotion
;; - Evil Matchit
;; - Evil Multiedit
;; - Evil Multiple cursors
;; - Evil Snipe
;; - Expand Region
;; - Flycheck
;; - Flycheck Pycheck
;; - Format All
;; - Git Gutter
;; - Hydra
;; - Highlight Thing
;; - LSP Mode
;; - Magit
;; - Origami
;; - Projectile
;; - Recentf
;; - Undo Tree
;; - Shell Pop
;; - Smart Jump
;; - Smartparens
;; - Windmove
;; - Yasnippet
;; - Tree sitter

;;; Code:
;; Customs
(defcustom emmacs-activate-lsp t
  "Non-nil means that LSP mode will be activated."
  :group 'emmacs
  :type 'boolean)

;; Evil
(use-package evil
  :init
  (setq evil-undo-system 'undo-tree)
  (evil-mode)
  :config
  ;; When evil :q[uit], close buffer and window instead of Emacs
  (evil-ex-define-cmd "q[uit]" 'kill-buffer-and-window)
  ;; When evil :wq, save and close buffer and window instead of Emacs
  (evil-ex-define-cmd "wq" 'save-and-kill-buffer-and-window)
  ;; Remap "," to repeat last macro (@@)
  (define-key evil-normal-state-map "," (kbd "@@"))
  (define-key evil-normal-state-map "\C-b" 'beginning-of-defun))

;; Dumb Jump
(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-prefer-searcher 'rg)
  (dumb-jump-mode))

;; Smart Jump
(use-package smart-jump
  :config
  (smart-jump-setup-default-registers)
  :bind (:map evil-motion-state-map
	      ("gd" . smart-jump-go)
	      ("gb" . smart-jump-back)
	      ("gl" . smart-jump-peek)))

;; Drag Stuff
(use-package drag-stuff
  :config
  (drag-stuff-define-keys)
  :hook
  (prog-mode . drag-stuff-mode))

;; Evil Multiedit
(defvar evil-multiedit-state-map (make-sparse-keymap))
;; preemptively fix a variable that is likely missing as well
(defvar evil-multiedit-insert-state-map (make-sparse-keymap))
(use-package evil-multiedit
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
	 ("C-p" . evil-multiedit-prev)))

(use-package evil-mc
  :hook
  (prog-mode . evil-mc-mode))

;; Recent files
(use-package recentf)

;; Rainbow mode
(use-package rainbow-mode)

;; Magit
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (require 'dash)
  (defmacro pretty-magit (WORD ICON COLOR &optional NO-PROMPT?)
    "Replace sanitized WORD with ICON, coloring it with COLOR."
    `(prog1
	 (add-to-list 'pretty-magit-alist
		      (list (rx bow (group ,WORD (eval (if ,NO-PROMPT? "" ":"))))
			    ,ICON ,COLOR))))

  (setq pretty-magit-alist nil)
  (pretty-magit "Add"    (all-the-icons-faicon "plus-circle") (face-foreground 'all-the-icons-blue))
  (pretty-magit "Fix"    (all-the-icons-octicon "bug")        (face-foreground 'all-the-icons-red))
  (pretty-magit "Doc"    (all-the-icons-octicon "file-text")  (face-foreground 'all-the-icons-green))
  (pretty-magit "Clean"  (all-the-icons-faicon  "scissors")   (face-foreground 'all-the-icons-yellow))
  (pretty-magit "Mod"    (all-the-icons-faicon  "wrench")     (face-foreground 'all-the-icons-purple))

  (defun add-magit-faces ()
    "Add face properties and compose symbols for buffer from pretty-magit."
    (interactive)
    (with-silent-modifications
      (--each pretty-magit-alist
	(-let (((rgx icon color) it))
	  (save-excursion
	    (goto-char (point-min))
	    (while (search-forward-regexp rgx nil t)
	      (compose-region
	       (match-beginning 1) (match-end 1) icon)
	      ;; (when props
	      (put-text-property
	       (match-beginning 1) (match-end 1) 'font-lock-face `(:inherit ,(get-text-property 0 'face icon)
									    :foreground ,color))))))))
  (advice-add 'magit-status :after 'add-magit-faces)
  (advice-add 'magit-refresh-buffer :after 'add-magit-faces))

;; Forge
(use-package forge
  :after magit)

;; Git time machine
(use-package git-timemachine
  :straight '(git-timemachine-mode-hook
	      :type git
	      :repo "https://codeberg.org/pidu/git-timemachine")
  :config
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

;; Avy
(use-package avy
  :bind* ("C-'" . avy-goto-word-1)
  :config
  (avy-setup-default)
  (setq avy-background t))

;; Beacon
(use-package beacon
  :config
  (beacon-mode 1))

;; Windmove
(use-package windmove
  :after hydra
  :config
  (defhydra hydra-window (global-map "C-x"
				     :color red
				     :hint nil)
    ("C-<left>"  windmove-left)
    ("C-<right>" windmove-right)
    ("C-<up>"    windmove-up)
    ("C-<down>"  windmove-down)))

;; Undo Tree
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-enable-undo-in-region nil)
  (defun undo-tree-split-side-by-side (original-function &rest args)
    "Split undo-tree side-by-side."
    (let ((split-height-threshold nil)
	  (split-width-threshold 0))
      (apply original-function args)))
  (advice-add 'undo-tree-visualize :around #'undo-tree-split-side-by-side))

;; Smartparens
(use-package smartparens
  :after hydra
  :config
  (sp-pair "(" nil :unless '(sp-point-before-word-p))
  (sp-pair "[" nil :unless '(sp-point-before-word-p))
  (sp-pair "{" nil :unless '(sp-point-before-word-p))
  (sp-pair "\"" nil :unless '(sp-point-before-word-p sp-point-after-word-p))
  (sp-pair "'" nil :unless '(sp-point-before-word-p sp-point-after-word-p))
  (sp-pair "`" nil :unless '(sp-point-before-word-p sp-point-after-word-p))
  (defhydra hydra-smartparens (:hint nil :color red)
    "
Moving^^^^                       Slurp & Barf^^    Wrapping^^                 Sexp juggling^^^^                Destructive^^^^        Exit
――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
 [_a_] beginning  [_n_] down    │ [_h_] bw slurp │ [_R_]   rewrap          │ [_S_] split   [_t_] transpose  │ [_c_] change inner  │ [_w_] copy
 [_e_] end        [_N_] bw down │ [_H_] bw barf  │ [_u_]   unwrap          │ [_s_] splice  [_A_] absorb     │ [_C_] change outer  │ [_g_] quit
 [_f_] forward    [_p_] up      │ [_l_] slurp    │ [_U_]   bw unwrap       │ [_r_] raise   [_E_] emit       │ [_k_] kill          │ [_q_] quit
 [_b_] backward   [_P_] bw up   │ [_L_] barf     │ [_(__{__[_] wrap (){}[]     │ [_j_] join    [_o_] convolute  │ [_K_] bw kill"
    ;; Moving
    ("a" sp-beginning-of-sexp)
    ("e" sp-end-of-sexp)
    ("f" sp-forward-sexp)
    ("b" sp-backward-sexp)
    ("n" sp-down-sexp)
    ("N" sp-backward-down-sexp)
    ("p" sp-up-sexp)
    ("P" sp-backward-up-sexp)
    
    ;; Slurping & barfing
    ("h" sp-backward-slurp-sexp)
    ("H" sp-backward-barf-sexp)
    ("l" sp-forward-slurp-sexp)
    ("L" sp-forward-barf-sexp)
    
    ;; Wrapping
    ("R" sp-rewrap-sexp)
    ("u" sp-unwrap-sexp)
    ("U" sp-backward-unwrap-sexp)
    ("(" sp-wrap-round)
    ("{" sp-wrap-curly)
    ("[" sp-wrap-square)
    
    ;; Sexp juggling
    ("S" sp-split-sexp)
    ("s" sp-splice-sexp)
    ("r" sp-raise-sexp)
    ("j" sp-join-sexp)
    ("t" sp-transpose-sexp)
    ("A" sp-absorb-sexp)
    ("E" sp-emit-sexp)
    ("o" sp-convolute-sexp)
    
    ;; Destructive editing
    ("c" sp-change-inner :exit t)
    ("C" sp-change-enclosing :exit t)
    ("k" sp-kill-sexp)
    ("K" sp-backward-kill-sexp)
    ("w" sp-copy-sexp)

    ;; Quitting
    ("<escape>" nil)
    ("C-x k" kill-buffer-and-window :exit t)
    ("q" nil)
    ("g" nil))
  :hook
  (python-mode . smartparens-mode)
  (emacs-lisp-mode . smartparens-mode)
  (c-mode . smartparens-mode)
  (c++-mode . smartparens-mode)
  (specman-mode . smartparens-mode)
  (js2-mode . smartparens-mode)
  (html-mode . smartparens-mode)
  :bind ((:map prog-mode-map
	       ("C-(" . hydra-smartparens/body))))

;; Highlight indent guides
(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'stack)
  :hook
  (prog-mode . highlight-indent-guides-mode))

;; Flycheck
(use-package flycheck
  :defer 3
  :init
  (global-flycheck-mode))

;; Expand Region
(use-package expand-region
  :bind
  ("C-0"  . 'er/expand-region))

;; Highlight thing
(use-package highlight-thing
  :config
  ;; (set-face-attribute 'highlight-thing nil
		      ;; :inherit 'region)
  (add-hook 'iedit-mode-hook (lambda()
			       (highlight-thing-mode -1)))

  (add-hook 'iedit-mode-end-hook (lambda()
				   (highlight-thing-mode 1)))
  (add-hook 'evil-visual-state-entry-hook (lambda()
					    (highlight-thing-mode -1)))
  (add-hook 'evil-visual-state-exit-hook (lambda()
					   (highlight-thing-mode 1)))
  :init
  (setq highlight-thing-what-thing 'symbol
	highlight-thing-delay-seconds 0.5
	highlight-thing-all-visible-buffers-p nil
	highlight-thing-limit-to-region-in-large-buffers-p t
	highlight-thing-narrow-region-lines 30)
  :hook
  (csv-mode . (lambda()
		(highlight-thing-mode 0)))
  (prog-mode . highlight-thing-mode)
  (specman-mode . highlight-thing-mode))

;; Column Enforce Mode
(use-package column-enforce-mode
  :hook
  (prog-mode . column-enforce-mode)
  (web-mode . (lambda()
		(column-enforce-mode 0)))
  :config
  (setq column-enforce-column 100))

;; Shell Pop
(use-package shell-pop
  :config
  (setq shell-pop-window-size 30)
  (evil-set-initial-state 'term-mode 'emacs)
  :bind (("<f5>" . shell-pop)))

;; Evil Snipe
(use-package evil-snipe
  :hook
  (prog-mode . evil-snipe-mode)
  (magit-mode . turn-off-evil-snipe-override-mode))

(when emmacs-activate-lsp
  ;; LSP Mode
  (use-package lsp-mode
    :init
    (add-to-list 'exec-path "/home/ema2159/anaconda3/envs/general/bin")
    :commands lsp
    :custom
    (lsp-keymap-prefix "C-c C-l")
    (lsp-auto-guess-root nil)
    (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
    :config
    (setq
     lsp-signature-auto-activate nil
     lsp-signature-render-documentation nil
     ;; lsp-ui-doc-use-childframe nil
     ;; lsp-ui-doc-use-webkit t
     )
    :hook
    (c-mode . lsp)
    (c++-mode . lsp)
    (python-mode . lsp)
    (js-mode . lsp)
    (typescript-mode . lsp)
    (tsx-mode . lsp)
    (css-mode . lsp)
    (web-mode . lsp)
    (rust-mode . lsp)
    (go-mode . lsp)
    (cmake-mode . lsp)
    (lsp-mode . lsp-headerline-breadcrumb-mode))

  ;; LSP Pyright
  (use-package lsp-pyright
    :ensure t
    :init
    (setq lsp-pyright-extra-paths ["/opt/ros/noetic/lib/python3/dist-packages"
                                   "/usr/lib/python3/dist-packages"])
    :config
    (setenv "NODE\_OPTIONS" "--experimental-worker")
    (setq lsp-pyright-venv-path "/home/ema2159/anaconda3/envs/general")
    :hook (python-mode . (lambda ()
			   (require 'lsp-pyright)
			   (lsp))))  ; or lsp-deferred

  ;; LSP UI
  (use-package lsp-ui
    :commands lsp-ui-mode
    :config
    (lsp-ui-sideline))

  ;; LSP Treemacs
  (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

  ;; LSP Ivy
  (use-package lsp-ivy))


;; Company Quick Help
(use-package company-quickhelp
  :defines company-quickhelp-delay
  :bind (:map company-active-map
	      ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
  :hook (global-company-mode . company-quickhelp-mode)
  :init (setq company-quickhelp-delay 0.5)
  (setq company-quickhelp-use-propertized-text t))

;; Company
(use-package company
  :defer 2
  :hook
  (after-init . global-company-mode)
  :config
  (company-mode 1)
  (setq company-idle-delay 0.3
	company-tooltip-align-annotations t
	company-require-match nil
	company-minimum-prefix-length 2
	company-dabbrev-ignore-case t
	company-dabbrev-downcase nil))

;; Yasnippet
(use-package yasnippet
  :defer t
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"))
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
  :bind
  ("C-x C-a" . ivy-yasnippet))

;; Hydra
(use-package hydra
  :defer 2
  :config
  (hydra-add-font-lock))

;; Evil Easymotion
(use-package evil-easymotion
  :after evil
  :config
  (evilem-default-keybindings "SPC")
  (evilem-define (kbd "SPC <up>") 'evil-previous-line)
  (evilem-define (kbd "SPC <down>") 'evil-next-line))

;; Projectile
(use-package projectile
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
  :after evil
  :hook
  (web-mode . turn-on-evil-matchit-mode)
  (html-mode . turn-on-evil-matchit-mode)
  (rjsx-mode . turn-on-evil-matchit-mode))

;; Origami
(use-package origami
  :hook
  (prog-mode . origami-mode)
  :config
  (defhydra hydra-origami (:color red)
    "
――――――――――――――――――――――――――――――――――――――――――――――――――――
 _o_pen node  │ _n_ext fold     │ _t_oggle node
 _c_lose node │ _p_revious fold │ toggle _a_ll
――――――――――――――――――――――――――――――――――――――――――――――――――――
  "
    ("o" origami-open-node)
    ("c" origami-close-node)
    ("n" origami-next-fold)
    ("p" origami-previous-fold)
    ("t" origami-toggle-node)
    ("a" origami-toggle-all-nodes)
    ("C-c o" nil :color blue))
  :bind ((:map origami-mode-map
	       ("C-c o" . hydra-origami/body))))

;; Format All
(use-package format-all)

;; Git Gutter
(use-package git-gutter
  :hook
  (prog-mode . git-gutter-mode))

;; Tree sitter
(use-package tree-sitter
  :hook
  (c-mode . tree-sitter-hl-mode)
  (c++-mode . tree-sitter-hl-mode)
  (python-mode . tree-sitter-hl-mode)
  (js-mode . tree-sitter-hl-mode)
  (ts-mode . tree-sitter-hl-mode)
  (tsx-mode . tree-sitter-hl-mode)
  (typescript-mode . tree-sitter-hl-mode)
  (typescript-tsx-mode . tree-sitter-hl-mode)
  (css-mode . tree-sitter-hl-mode)
  (web-mode . tree-sitter-hl-mode)
  (rust-mode . tree-sitter-hl-mode)
  (go-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(provide '3_emmacs_tools)
;;; 3_emmacs_tools.el ends here
