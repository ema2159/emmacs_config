;;; 4_emmacs_modes.el --- Modes configuration

;;; Commentary:
;; In this section the following packages are loaded:
;; - Anaconda Mode
;; - CSV Mode
;; - Elpy
;; - Emmet Mode
;; - Irony
;; - Js2 Mode
;; - Specman Mode
;; - Tern Mode

;;; Code:
;; Specman Mode
(add-to-list 'load-path "~/.emacs.d/specman")
(put 'specman-mode 'derived-mode-parent 'prog-mode)
(load "specman-mode")
(add-to-list 'auto-mode-alist '("\\.e\\'" . specman-mode))
(add-to-list 'auto-mode-alist '("\\.ecom\\'" . specman-mode))
(add-hook 'specman-mode-hook (lambda () (use-local-map nil)))
(add-hook 'specman-mode-hook 'yas-minor-mode)

;; Irony
(use-package irony
  :ensure t
  :hook
  ((c-mode . irony-mode)
   (c++-mode . irony-mode)
   (irony-mode . irony-cdb-autosetup-compile-options)))

;; Csv Mode
(use-package csv-mode
  :ensure t
  :mode
  ("\\.[Cc][Ss][Vv]\\'" . csv-mode))

;; Tern Mode
(use-package tern
  :ensure t
  :after js-mode)

;; Js2 Mode
(use-package js2-mode
  :ensure t
  :mode
  ("\\.js\\'" . js2-mode)
  ("node" . js2-mode)
  :config
  (add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode))))

;; Anaconda Mode
(use-package anaconda-mode
  :ensure t
  :config
  (pythonic-activate "/home/emmanuel/anaconda3/envs/ml")
  :hook
  (python-mode . anaconda-mode))

;; Emmet Mode
(use-package emmet-mode
  :ensure t
  :hook
  (sgml-mode . emmet-mode)
  (css-mode . emmet-mode))

;; ESS
(use-package ess
  :ensure t)

;; Go Mode
(use-package go-mode
  :ensure t)

(provide '4_emmacs_modes)
;;; 4_emmacs_modes.el ends here
