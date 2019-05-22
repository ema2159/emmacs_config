;;; 2_emmacs-ui.el --- User Insterface

;;; Commentary:
;; In this section the following packages are loaded:
;; - All The Icons Ivy
;; - Solaire Mode

;;; Code:
;; All The Icons Ivy
(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup))

;; Solaire mode
(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

(provide '2_emmacs_ui)
;;; 2_emmacs-ui.el ends here
