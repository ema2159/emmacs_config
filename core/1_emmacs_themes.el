;;; 1_emmacs_themes.el --- Themes

;;; Commentary:
;; In this section the following packages are loaded:
;; - Atom One Dark Theme
;; - Doom Themes
;; - Spacemacs Theme

;;; Code:
(defcustom emmacs-theme-package 0
  "Theme to be loaded.
0 - Doom Themes
1 - Kaolin Themes
2 - Spacemacs Theme
3 - Atom One Dark
4 - Zenburn"
  :group 'emmacs
  :type 'integer)

(defcustom emmacs-treemacs-theme 1
  "Treemacs theme to be loaded.
0 - Doom Themes
1 - Doom Themes colorful
2 - Kaolin Themes"
  :group 'emmacs
  :type 'integer)

(defcustom emmacs-theme-number 32
  "Available themes.
| Doom                    | Kaolin                 | Spacemacs         |
|-------------------------+------------------------+-------------------|
| 0 doom-one              | 0 kaolin-eclipse       | 0 spacemacs-dark  |
| 1 doom-one-light        | 1 kaolin-aurora        | 1 spacemacs-light |
| 2 doom-city-lights      | 2 kaolin-ocean         |                   |
| 3 doom-molokai          | 3 kaolin-bubblegum     |                   |
| 4 doom-dracula          | 4 kaolin-galaxy        |                   |
| 5 doom-Iosvkem          | 5 kaolin-valley-dark   |                   |
| 6 doom-opera            | 6 kaolin-temple        |                   |
| 7 doom-solarized-light  | 7 kaolin-dark          |                   |
| 8 doom-challenger-deep  | 8 kaolin-breeze        |                   |
| 9 doom-nord-light       | 9 kaolin-light         |                   |
| 10 doom-nord            | 10 kaolin-mono-dark    |                   |
| 11 doom-nova            | 11 kaolin-valley-light |                   |
| 12 doom-opera-light     |                        |                   |
| 13 doom-opera           |                        |                   |
| 14 doom-peacock         |                        |                   |
| 15 doom-solarized-light |                        |                   |
| 16 doom-sourcerer       |                        |                   |
| 17 doom-spacegrey       |                        |                   |
| 18 doom-tomorrow-day    |                        |                   |
| 19 doom-tomorrow-night  |                        |                   |
| 20 doom-vibrant         |                        |                   |
| 21 doom-solarized-dark  |                        |                   |
| 22 doom-gruvbox         |                        |                   |
| 23 doom-fairy-floss     |                        |                   |
| 24 doom-outrun-electric |                        |                   |
| 25 doom-snazzy          |                        |                   |
| 26 doom-palenight       |                        |                   |
| 27 doom-moonlight       |                        |                   |
| 28 doom-wilmersdorf     |                        |                   |
| 29 doom-oceanic-next    |                        |                   |
| 30 doom-laserwave       |                        |                   |
| 31 doom-material        |                        |                   |
| 32 doom-dark+           |                        |                   |
| 33 doom-horizon         |                        |                   |
| 34 doom-manegarm        |                        |                   |
|-------------------------+------------------------+-------------------|"
  :group 'emmacs
  :type 'integer)

;; Theme switch
(cond
 ((eq emmacs-theme-package 0)
  ;; Doom themes
  (use-package doom-themes
    :config
    (doom-themes-org-config)
    (cond
     ((eq emmacs-theme-number 0) (load-theme 'doom-one t))
     ((eq emmacs-theme-number 1) (load-theme 'doom-one-light t))
     ((eq emmacs-theme-number 2) (load-theme 'doom-city-lights t))
     ((eq emmacs-theme-number 3) (load-theme 'doom-molokai t))
     ((eq emmacs-theme-number 4) (load-theme 'doom-dracula t))
     ((eq emmacs-theme-number 5) (load-theme 'doom-Iosvkem t))
     ((eq emmacs-theme-number 6) (load-theme 'doom-opera t))
     ((eq emmacs-theme-number 7) (load-theme 'doom-solarized-light t))
     ((eq emmacs-theme-number 8) (load-theme 'doom-challenger-deep t))
     ((eq emmacs-theme-number 9) (load-theme 'doom-nord-light t))
     ((eq emmacs-theme-number 10) (load-theme 'doom-nord t))
     ((eq emmacs-theme-number 11) (load-theme 'doom-nova t))
     ((eq emmacs-theme-number 12) (load-theme 'doom-opera-light t))
     ((eq emmacs-theme-number 13) (load-theme 'doom-opera t))
     ((eq emmacs-theme-number 14) (load-theme 'doom-peacock t))
     ((eq emmacs-theme-number 15) (load-theme 'doom-solarized-light t))
     ((eq emmacs-theme-number 16) (load-theme 'doom-sourcerer t))
     ((eq emmacs-theme-number 17) (load-theme 'doom-spacegrey t))
     ((eq emmacs-theme-number 18) (load-theme 'doom-tomorrow-day t))
     ((eq emmacs-theme-number 19) (load-theme 'doom-tomorrow-night t))
     ((eq emmacs-theme-number 20) (load-theme 'doom-vibrant t))
     ((eq emmacs-theme-number 21) (load-theme 'doom-solarized-dark t))
     ((eq emmacs-theme-number 22) (load-theme 'doom-gruvbox t))
     ((eq emmacs-theme-number 23) (load-theme 'doom-fairy-floss t))
     ((eq emmacs-theme-number 24) (load-theme 'doom-outrun-electric t))
     ((eq emmacs-theme-number 25) (load-theme 'doom-snazzy t))
     ((eq emmacs-theme-number 26) (load-theme 'doom-palenight t))
     ((eq emmacs-theme-number 27) (load-theme 'doom-moonlight t))
     ((eq emmacs-theme-number 28) (load-theme 'doom-wilmersdorf t))
     ((eq emmacs-theme-number 29) (load-theme 'doom-oceanic-next t))
     ((eq emmacs-theme-number 30) (load-theme 'doom-laserwave t))
     ((eq emmacs-theme-number 31) (load-theme 'doom-material t))
     ((eq emmacs-theme-number 32) (load-theme 'doom-dark+ t))
     ((eq emmacs-theme-number 33) (load-theme 'doom-horizon t))
     ((eq emmacs-theme-number 34) (load-theme 'doom-manegarm t))
     )))
 ((eq emmacs-theme-package 1)
  ;; Kaolin Themes
  (use-package kaolin-themes
    :config
    (cond
     ((eq emmacs-theme-number 0) (load-theme 'kaolin-eclipse t))
     ((eq emmacs-theme-number 1) (load-theme 'kaolin-aurora t))
     ((eq emmacs-theme-number 2) (load-theme 'kaolin-ocean t))
     ((eq emmacs-theme-number 3) (load-theme 'kaolin-bubblegum t))
     ((eq emmacs-theme-number 4) (load-theme 'kaolin-galaxy t))
     ((eq emmacs-theme-number 5) (load-theme 'kaolin-valley-dark t))
     ((eq emmacs-theme-number 6) (load-theme 'kaolin-temple t))
     ((eq emmacs-theme-number 7) (load-theme 'kaolin-dark t))
     ((eq emmacs-theme-number 8) (load-theme 'kaolin-breeze t))
     ((eq emmacs-theme-number 9) (load-theme 'kaolin-light t))
     ((eq emmacs-theme-number 10) (load-theme 'kaolin-mono-dark t))
     ((eq emmacs-theme-number 11) (load-theme 'kaolin-valley-light t))
     )))
 ((eq emmacs-theme-package 2)
  (cond
   ((eq emmacs-theme-number 0) (use-package spacemacs-dark-theme
				 :config
				 (load-theme 'spacemacs-dark t)))
   ((eq emmacs-theme-number 1) (use-package spacemacs-light-theme
				 :config
				 (load-theme 'spacemacs-light t)))))
 ((eq emmacs-theme-package 3)
  (use-package atom-one-dark-theme
    :config
    (load-theme 'atom-one-dark t))
  )
 ((eq emmacs-theme-package 4)
  (use-package zenburn-theme
    :config
    (load-theme 'zenburn t))))

(cond ((eq emmacs-treemacs-theme 0)
       (use-package doom-themes
	 :config
	 (doom-themes-treemacs-config)))
      ((eq emmacs-treemacs-theme 1)
       (use-package doom-themes
	 :config
	 (setq doom-themes-treemacs-theme "doom-colors")
	 (doom-themes-treemacs-config)))
      ((eq emmacs-treemacs-theme 2)
       ;; Kaolin Themes
       (use-package kaolin-themes
	 :config
	 (kaolin-treemacs-theme))))
      
 (provide '1_emmacs_themes)
;;; 1_emmacs_themes.el ends here
