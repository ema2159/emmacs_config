;;; 4_emmacs_modes.el --- Modes configuration

;;; Commentary:
;; In this section the following packages are loaded:
;; - Anaconda Mode
;; - CSV Mode
;; - Elpy
;; - Emmet Mode
;; - Go Mode
;; - Js2 Mode
;; - Rjsx Mode
;; - Simple HTTPD
;; - Skewer Mode
;; - Tern Mode

;;; Code:

;; Irony
(use-package irony
  :hook
  ((c-mode . irony-mode)
   (c++-mode . irony-mode)
   (irony-mode . irony-cdb-autosetup-compile-options)))

;; Csv Mode
(use-package csv-mode
  :mode
  ("\\.[Cc][Ss][Vv]\\'" . csv-mode))

;; Tern Mode
(use-package tern
  :after js-mode)

;; Js2 Mode
;; (use-package js2-mode
;;   :mode
;;   ("\\.js\\'" . js2-mode)
;;   ("node" . js2-mode)
;;   :config
;;   (setq js-indent-level 2)
;;   (add-hook 'js2-mode-hook (lambda ()
;;                            (tern-mode)
;;                            (company-mode))))

;; rjsx Mode
(use-package rjsx-mode
  :mode
  ("\\.js\\'" . rjsx-mode)
  ("node" . rjsx-mode)
  :bind
  (("C-j" . emmet-expand-line))
  :hook
  (rjsx-mode . yas-minor-mode)
  :config
  (setq js-indent-level 2)
  (add-hook 'rjsx-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode))))

;; Anaconda Mode
(use-package anaconda-mode
  :config
  (pythonic-activate "/home/ema2159/anaconda3/envs/general")
  :hook
  (python-mode . anaconda-mode))

;; Emmet Mode
(use-package emmet-mode
  :hook
  (rjsx-mode . emmet-mode)
  (sgml-mode . emmet-mode)
  (css-mode . emmet-mode))

;; ESS
(use-package ess)

;; Go Mode
(use-package go-mode)

;; Skewer mode
(use-package skewer-mode
  :hook
  (rjsx-mode . skewer-mode)
  (js2-mode . skewer-mode)
  (css-mode . skewer-css-mode)
  (web-mode . skewer-mode)
  (mhtml-mode . skewer-html-mode))

;; Impatient mode
(use-package impatient-mode)

;; Simple HTTPD
(use-package simple-httpd
  :config
  (setq httpd-root "/var/www"))

;; Markdown
(use-package markdown-mode)

;; Web Mode
(use-package web-mode
  :mode
  ("\\.html$" . web-mode)
  ("\\.ejs$" . web-mode)
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq js-indent-level 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t))

;; Css Mode
(use-package css-mode
  :straight nil
  :config
  (setq css-indent-offset 2))

(provide '4_emmacs_modes)
;;; 4_emmacs_modes.el ends here
