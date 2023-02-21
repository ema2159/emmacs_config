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
;; - GLSL Mode
;; - CUDA Mode
;; - CMake Mode

;;; Code:

;; Csv Mode
(use-package csv-mode
  :mode
  ("\\.[Cc][Ss][Vv]\\'" . csv-mode))

;; rjsx Mode
(use-package rjsx-mode
  :mode
  ;; ("\\.js\\'" . rjsx-mode)
  ("node" . rjsx-mode)
  :bind
  (("C-j" . emmet-expand-line))
  :hook
  (rjsx-mode . yas-minor-mode)
  :config
  (setq js-indent-level 2)
  (add-hook 'rjsx-mode-hook (lambda ()
                           ;; (tern-mode)
                           (company-mode))))

;; Javascript
(add-hook 'js-mode-hook (lambda()
			  (setq indent-tabs-mode nil
				js-indent-level 2)))

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
  ;; ("\\.js$" . web-mode)
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
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :config
  (setq typescript-indent-level 4)
  (add-hook 'typescript-mode #'subword-mode)
  :mode
  ("\\.ts\\'" . typescript-mode)
  ("\\.tsx\\'" . typescript-tsx-mode))

;; Css Mode
(use-package css-mode
  :straight nil
  :config
  (setq css-indent-offset 2))

;; GLSL Mode
(use-package glsl-mode
  :mode
  ("\\.glsl$" . glsl-mode)
  ("\\.vert$" . glsl-mode)
  ("\\.tesc$" . glsl-mode)
  ("\\.tese$" . glsl-mode)
  ("\\.geom$" . glsl-mode)
  ("\\.frag$" . glsl-mode)
  ("\\.comp$" . glsl-mode))

;; CUDA Mode
(use-package cuda-mode
  :mode
  ("\\.cu$" . cuda-mode)
  :hook
  (cuda-mode . (lambda ()
		 (setq c-basic-offset 4
		       flycheck-cuda-include-path (list "."))
		 (run-hooks 'prog-mode-hook))))

;; CMake Mode
(use-package cmake-mode
  :mode
  ("\\.cmake$" . cmake-mode)
  ("CMakeLists.txt$" . cmake-mode)
  )

(use-package dockerfile-mode
  :mode
  ("Dockerfile" . dockerfile-mode))

(provide '4_emmacs_modes)
;;; 4_emmacs_modes.el ends here
