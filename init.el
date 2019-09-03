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
 '(ansi-color-names-vector
   ["#1D252C" "#D95468" "#8BD49C" "#EBBF83" "#5EC4FF" "#E27E8D" "#70E1E8" "#A0B3C5"])
 '(custom-safe-themes
   (quote
    ("db10381a554231a40b7474eaac28bd58f05067faacce3b25d294bb179a3511a1" "9c27124b3a653d43b3ffa088cd092c34f3f82296cf0d5d4f719c0c0817e1afa6" "9de6237a2054e206100e42c04b3b7e72c2ec94f0fbc55f03dd28cec544940cd1" "fd944f09d4d0c4d4a3c82bd7b3360f17e3ada8adf29f28199d09308ba01cc092" default "muse-solarized-dark")))
 '(fci-rule-color "#56697A")
 '(jdee-db-active-breakpoint-face-colors (cons "#10151C" "#5EC4FF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#10151C" "#8BD49C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#10151C" "#384551"))
 '(objed-cursor-color "#D95468")
 '(package-selected-packages
   (quote
    (smart-jump zenburn-theme package-lint go-mode kaolin-themes spacemacs-theme skewer-mode dashboard esup highlight-thing expand-region column-enforce-mode dumb-jump smartparens-config which-key use-package treemacs-magit treemacs-icons-dired treemacs-evil solarized-theme solaire-mode shell-pop org-bullets linum-relative ivy-yasnippet irony flycheck evil-multiedit drag-stuff doom-themes dired-subtree dired-single dired-filter counsel beacon)))
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(tramp-ssh-controlmaster-options
   "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p -o ControlMaster=auto -o ControlPersist=yes" t)
 '(vc-annotate-background "#1D252C")
 '(vc-annotate-color-map
   (list
    (cons 20 "#8BD49C")
    (cons 40 "#abcd93")
    (cons 60 "#cbc68b")
    (cons 80 "#EBBF83")
    (cons 100 "#e5ae6f")
    (cons 120 "#df9e5b")
    (cons 140 "#D98E48")
    (cons 160 "#dc885f")
    (cons 180 "#df8376")
    (cons 200 "#E27E8D")
    (cons 220 "#df7080")
    (cons 240 "#dc6274")
    (cons 260 "#D95468")
    (cons 280 "#b05062")
    (cons 300 "#884c5c")
    (cons 320 "#604856")
    (cons 340 "#56697A")
    (cons 360 "#56697A")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-ivy-dir-face ((t (:inherit (font-lock-doc-face bold)))))
 '(specman-punctuation-face ((t (:inherit font-lock-keyword-face)))))
