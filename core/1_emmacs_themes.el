;;; 1_emmacs_themes.el --- Themes

;;; Commentary:
;; In this section the following packages are loaded:
;; - Atom One Dark Theme
;; - Doom Themes
;; - Solarized Theme
;; - Spacemacs Theme

;;; Code:
;; Doom themes
(use-package doom-themes
  :ensure t
  :defer t
  :config
  (doom-themes-org-config))

;; (use-package kaolin-themes
;;   :ensure t
;;   :defer t
;;   :config
;;   (kaolin-treemacs-theme))

;; Solarized theme
;; (use-package solarized-theme
  ;; :ensure t
  ;; :defer t)

;; Atom One Dark theme
;; (use-package atom-one-dark-theme
  ;; :ensure t
  ;; :defer t)

;; (require 'spacemacs-dark-theme)
;; (setq spacemacs-theme-comment-bg nil)
;; (require 'atom-one-dark-theme)
;; (require 'solarized-dark-theme)
(require 'doom-themes)
;; (require 'kaolin-themes)

;; Theme
(if (display-graphic-p)
    (progn
      ;; if graphic
      ;; (load-theme 'spacemacs-dark t))
      ;; (load-theme 'solarized-dark t))
      ;; (load-theme 'doom-one t))
      ;; (load-theme 'doom-city-lights t))
      ;; (load-theme 'doom-molokai t))
      ;; (load-theme 'kaolin-eclipse t))
      (load-theme 'doom-dracula t))
  ;; (load-theme 'doom-Iosvkem t))
  ;; (load-theme 'doom-solarized-dark t))
  ;; (load-theme 'atom-one-dark t))
  ;; else (optional)
  (load-theme 'atom-one-dark t))

(provide '1_emmacs_themes)
;;; 1_emmacs_themes.el ends here
