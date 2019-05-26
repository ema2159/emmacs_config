;;; 5_emmacs_org.el --- Org configuration

;;; Commentary:
;; In this section, Org mode and related packages are configured

;;; Code:

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

(provide '5_emmacs_org)
;;; 5_emmacs_org.el ends here
