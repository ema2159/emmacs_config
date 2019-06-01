;;; 4_emmacs_modes.el --- Modes configuration

;;; Commentary:
;; In this section the following packages are loaded:
;; - CSV Mode
;; - Elpy
;; - Irony
;; - Js2-mode
;; - Specman mode
;; - Tern mode

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

;; Csv-mode
(use-package csv-mode
  :ensure t
  :mode
  ("\\.[Cc][Ss][Vv]\\'" . csv-mode))

;; Tern mode
(use-package tern
  :ensure t
  :after js-mode)

;; Js2-mode
(use-package js2-mode
  :ensure t
  :mode
  ("\\.js\\'" . js2-mode)
  ("node" . js2-mode)
  :config
  (add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode))))

;; ESS
;; (use-package ess
;;   :ensure t)

(provide '4_emmacs_modes)
;;; 4_emmacs_modes.el ends here
