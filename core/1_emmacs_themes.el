;;; 1_emmacs_themes.el --- Themes

;;; Commentary:
;; In this section the following packages are loaded:
;; - Atom One Dark Theme
;; - Doom Themes
;; - Spacemacs Theme

;;; Code:
(defvar theme-package 1
  "Theme to be loaded.
0 - Doom Themes
1 - Kaolin Themes
2 - Spacemacs Theme")
(defvar theme-number 0
  "Available themes.
| Doom                   | Kaolin               | Spacemacs         |
|------------------------+----------------------+-------------------|
| 0 doom-one             | 0 kaolin-eclipse     | 0 spacemacs-dark  |
| 1 doom-one-light       | 1 kaolin-aurora      | 1 spacemacs-light |
| 2 doom-city-lights     | 2 kaolin-ocean       |                   |
| 3 doom-molokai         | 3 kaolin-bubblegum   |                   |
| 4 doom-dracula         | 4 kaolin-galaxy      |                   |
| 5 doom-Iosvkem         | 5 kaolin-valley-dark |                   |
| 6 doom-opera           | 6 kaolin-temple      |                   |
| 7 doom-solarized-light | 7 kaolin-dark        |                   |
|------------------------+----------------------+-------------------|"
  )

(cond
 ((eq theme-package 0)
  ;; Doom themes
  (use-package doom-themes
    :ensure t
    :config
    (doom-themes-treemacs-config)
    (doom-themes-org-config)
    (cond
     ((eq theme-number 0) (load-theme 'doom-one t))
     ((eq theme-number 1) (load-theme 'doom-one-light t))
     ((eq theme-number 2) (load-theme 'doom-city-lights t))
     ((eq theme-number 3) (load-theme 'doom-molokai t))
     ((eq theme-number 4) (load-theme 'doom-dracula t))
     ((eq theme-number 5) (load-theme 'doom-Iosvkem t))
     ((eq theme-number 6) (load-theme 'doom-opera t))
     ((eq theme-number 7) (load-theme 'doom-solarized-dark t)))
    ))
 ((eq theme-package 1)
  ;; Kaolin Themes
  (use-package kaolin-themes
    :ensure t
    :config
    (kaolin-treemacs-theme)
    ;; (setq kaolin-themes-treemacs-hl-line t)
    (cond
     ((eq theme-number 0) (load-theme 'kaolin-eclipse t))
     ((eq theme-number 1) (load-theme 'kaolin-aurora t))
     ((eq theme-number 2) (load-theme 'kaolin-ocean t))
     ((eq theme-number 3) (load-theme 'kaolin-bubblegum t))
     ((eq theme-number 4) (load-theme 'kaolin-galaxy t))
     ((eq theme-number 5) (load-theme 'kaolin-valley-dark t))
     ((eq theme-number 6) (load-theme 'kaolin-temple t))
     ((eq theme-number 7) (load-theme 'kaolin-dark t))
     )))
 ((eq theme-package 2)
  (cond
   ((eq theme-number 0) (use-package spacemacs-dark-theme
					    :config
					    (load-theme 'spacemacs-dark t)))
   ((eq theme-number 1) (use-package spacemacs-light-theme
			  :config
			  (load-theme 'spacemacs-light t)))))
 ((eq theme-package 3)
  (cond
   (use-package atom-one-dark
     :config
     (load-theme 'atom-one-dark t))
   )))
 
 (provide '1_emmacs_themes)
;;; 1_emmacs_themes.el ends here
