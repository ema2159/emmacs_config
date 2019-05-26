;;; 4_emmacs_modes.el --- Modes configuration

;;; Commentary:
;; In this section the following packages are loaded:
;; - CSV Mode
;; - Elpy
;; - Irony
;; - Specman mode

;;; Code:
;; Specman mode
(add-to-list 'load-path "~/.emacs.d/specman")
(load "specman-mode")
(put 'specman-mode 'derived-mode-parent 'prog-mode)
(add-to-list 'auto-mode-alist '("\\.e\\'" . specman-mode))
(add-to-list 'auto-mode-alist '("\\.ecom\\'" . specman-mode))
(add-hook 'specman-mode-hook (lambda () (use-local-map nil)))
(add-hook 'specman-mode-hook 'yas-minor-mode)

;; Elpy
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

;; Irnony
(use-package irony
  :ensure t
  :hook
  ((c-mode . irony-mode)
   (c++-mode . irony-mode)
   (irony-mode . irony-cdb-autosetup-compile-options)))

;; Csv-mode configuration
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

(provide '4_emmacs_modes)
;;; 4_emmacs_modes.el ends here
