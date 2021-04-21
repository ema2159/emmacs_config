;;; 4_emmacs_modes.el --- Modes configuration

;;; Commentary:
;; In this section the following packages are loaded:
;; - CSV Mode
;; - Elpy
;; - Emmet Mode
;; - Go Mode
;; - Js2 Mode
;; - Rjsx Mode
;; - Simple HTTPD
;; - Skewer Mode
;; - Rust Mode
;; - Typescript Mode

;;; Code:

;; Csv Mode
(use-package csv-mode
  :mode
  ("\\.[Cc][Ss][Vv]\\'" . csv-mode))

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

;; Rust Mode
(use-package rust-mode)

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

;; Typescript Mode
(use-package typescript-mode
  :mode
  ("\\.ts\\'" . typescript-mode)
  ("\\.tsx\\'" . typescript-mode))

;; Css Mode
(use-package css-mode
  :straight nil
  :config
  (setq css-indent-offset 2))

(provide '4_emmacs_modes)
;;; 4_emmacs_modes.el ends here
