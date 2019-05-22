;;; emmacs-ui.el --- User Insterface

;;; Commentary:
;; In this section the following packages are loaded:
;; - All The Icons Ivy
;; - Atom One Dark Theme
;; - Doom Themes
;; - Solaire Mode
;; - Solarized Theme
;; - Spacemacs Theme

;;; Code:
;; Solaire mode
(use-package solaire-mode
  :ensure t
  :config
  (solaire-mode-swap-bg)
  (solaire-global-mode +1))

;; Doom themes
(use-package doom-themes
  :ensure t)

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

(solaire-mode-swap-bg)
(doom-themes-org-config)

(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup))

(provide 'emmacs_ui)
;;; emmacs-ui.el ends here
