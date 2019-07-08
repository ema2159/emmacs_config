;;; 5_emmacs_org.el --- Org configuration

;;; Commentary:
;; In this section, Org mode and related packages are configured

;;; Code:

;; Org mode
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :config
  ;; Org mouse for checkboxes
  (require 'org-mouse)
  (setq org-agenda-files (list "~/org/week.org"))
  (setq org-support-shift-select t)
  ;; Hide markers for italic, bold etc
  (setq org-hide-emphasis-markers t)
  ;; Print timestamp when done
  (setq org-log-done 'time)
  ;; Handle source blocks
  (setq org-src-tab-acts-natively t)
  ;; Make LaTex use minted when exporting
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process
	'("pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"))
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :hook
  (org-mode . toggle-word-wrap)
  (org-mode . toggle-truncate-lines))

;; Org bullets
(use-package org-bullets
  :ensure t
  :defer t
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

;; Org bullets
(use-package toc-org
  :ensure t
  :hook
  (org-mode-hook . toc-org-mode))

(provide '5_emmacs_org)
;;; 5_emmacs_org.el ends here
