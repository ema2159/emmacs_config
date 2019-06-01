
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

;; (setq use-package-always-defer t
;;       use-package-always-ensure t
;;       use-package-compute-statistics t)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (skewer-mode dashboard esup highlight-thing expand-region column-enforce-mode dumb-jump smartparens-config which-key use-package treemacs-magit treemacs-icons-dired treemacs-evil tabbar-ruler solarized-theme solaire-mode shell-pop org-bullets linum-relative ivy-yasnippet irony flycheck evil-multiedit elpy drag-stuff doom-themes dired-subtree dired-single dired-filter counsel beacon)))
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(tramp-ssh-controlmaster-options
   "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p -o ControlMaster=auto -o ControlPersist=yes" t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
