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
  :config
  (doom-themes-org-config))

;; Solarized theme
(use-package solarized-theme
  :ensure t)

;; Atom One Dark theme
(use-package atom-one-dark-theme
  :ensure t)

;; (require 'spacemacs-dark-theme)
;; (require 'solarized-dark-theme)
;; (require 'doom-one-theme)
;; (require 'doom-city-lights-theme)
(require 'doom-dracula-theme)
;; (require 'atom-one-dark-theme)

;; Theme
(if (display-graphic-p)
    (progn
    ;; if graphic
      ;; (load-theme 'spacemacs-dark t))
      ;; (load-theme 'solarized-dark t))
      ;; (load-theme 'doom-one t))
      ;; (load-theme 'doom-city-lights t))
      (load-theme 'doom-dracula t))
      ;; (load-theme 'atom-one-dark t))
    ;; else (optional)
  (load-theme 'atom-one-dark t))

(provide '1_emmacs_themes)
;;; 1_emmacs_themes.el ends here
