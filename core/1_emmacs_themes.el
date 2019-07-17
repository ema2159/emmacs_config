;;; 1_emmacs_themes.el --- Themes

;;; Commentary:
;; In this section the following packages are loaded:
;; - Atom One Dark Theme
;; - Doom Themes
;; - Spacemacs Theme

;;; Code:
(defvar emmacs-theme-package 0
  "Theme to be loaded.
0 - Doom Themes
1 - Kaolin Themes
2 - Spacemacs Theme
3 - Atom One Dark
4 - Base16
5 - Zenburn")

(defvar emmacs-treemacs-theme 0
  "Treemacs theme to be loaded.")

(defvar emmacs-theme-number 21
  "Available themes.
| Doom                    | Kaolin                 | Spacemacs         | Base16                              |
|-------------------------+------------------------+-------------------+-------------------------------------|
| 0 doom-one              | 0 kaolin-eclipse       | 0 spacemacs-dark  | 1 base16-3024                       |
| 1 doom-one-light        | 1 kaolin-aurora        | 1 spacemacs-light | 2 base16-apathy                     |
| 2 doom-city-lights      | 2 kaolin-ocean         |                   | 3 base16-ashes                      |
| 3 doom-molokai          | 3 kaolin-bubblegum     |                   | 4 base16-atelier-cave-light         |
| 4 doom-dracula          | 4 kaolin-galaxy        |                   | 5 base16-atelier-cave               |
| 5 doom-Iosvkem          | 5 kaolin-valley-dark   |                   | 6 base16-atelier-dune-light         |
| 6 doom-opera            | 6 kaolin-temple        |                   | 7 base16-atelier-dune               |
| 7 doom-solarized-light  | 7 kaolin-dark          |                   | 8 base16-atelier-estuary-light      |
| 8 doom-challenger-deep  | 8 kaolin-breeze        |                   | 9 base16-atelier-estuary            |
| 9 doom-nord-light       | 9 kaolin-light         |                   | 10 base16-atelier-forest-light      |
| 10 doom-nord            | 10 kaolin-mono-dark    |                   | 11 base16-atelier-forest            |
| 11 doom-nova            | 11 kaolin-valley-light |                   | 12 base16-atelier-heath-light       |
| 12 doom-opera-light     |                        |                   | 13 base16-atelier-heath             |
| 13 doom-opera           |                        |                   | 14 base16-atelier-lakeside-light    |
| 14 doom-peacock         |                        |                   | 15 base16-atelier-lakeside          |
| 15 doom-solarized-light |                        |                   | 16 base16-atelier-plateau-light     |
| 16 doom-sourcerer       |                        |                   | 17 base16-atelier-plateau           |
| 17 doom-spacegrey       |                        |                   | 18 base16-atelier-savanna-light     |
| 18 doom-tomorrow-day    |                        |                   | 19 base16-atelier-savanna           |
| 19 doom-tomorrow-night  |                        |                   | 20 base16-atelier-seaside-light     |
| 20 doom-vibrant         |                        |                   | 21 base16-atelier-seaside           |
|                         |                        |                   | 22 base16-atelier-sulphurpool-light |
|                         |                        |                   | 23 base16-atelier-sulphurpool       |
|                         |                        |                   | 24 base16-atlas                     |
|                         |                        |                   | 25 base16-bespin                    |
|                         |                        |                   | 26 base16-black-metal-bathory       |
|                         |                        |                   | 27 base16-black-metal-burzum        |
|                         |                        |                   | 28 base16-black-metal-dark-funeral  |
|                         |                        |                   | 29 base16-black-metal-gorgoroth     |
|                         |                        |                   | 30 base16-black-metal-immortal      |
|                         |                        |                   | 31 base16-black-metal-khold         |
|                         |                        |                   | 32 base16-black-metal-marduk        |
|                         |                        |                   | 33 base16-black-metal-mayhem        |
|                         |                        |                   | 34 base16-black-metal-nile          |
|                         |                        |                   | 35 base16-black-metal               |
|                         |                        |                   | 36 base16-black-metal-venom         |
|                         |                        |                   | 37 base16-brewer                    |
|                         |                        |                   | 38 base16-bright                    |
|                         |                        |                   | 39 base16-brogrammer                |
|                         |                        |                   | 40 base16-brushtrees-dark           |
|                         |                        |                   | 41 base16-brushtrees                |
|                         |                        |                   | 42 base16-chalk                     |
|                         |                        |                   | 43 base16-circus                    |
|                         |                        |                   | 44 base16-classic-dark              |
|                         |                        |                   | 45 base16-classic-light             |
|                         |                        |                   | 46 base16-codeschool                |
|                         |                        |                   | 47 base16-cupcake                   |
|                         |                        |                   | 48 base16-cupertino                 |
|                         |                        |                   | 49 base16-darktooth                 |
|                         |                        |                   | 50 base16-default-dark              |
|                         |                        |                   | 51 base16-default-light             |
|                         |                        |                   | 52 base16-dracula                   |
|                         |                        |                   | 53 base16-eighties                  |
|                         |                        |                   | 54 base16-embers                    |
|                         |                        |                   | 55 base16-flat                      |
|                         |                        |                   | 56 base16-fruit-soda                |
|                         |                        |                   | 57 base16-github                    |
|                         |                        |                   | 58 base16-google-dark               |
|                         |                        |                   | 59 base16-google-light              |
|                         |                        |                   | 60 base16-grayscale-dark            |
|                         |                        |                   | 61 base16-grayscale-light           |
|                         |                        |                   | 62 base16-greenscreen               |
|                         |                        |                   | 63 base16-gruvbox-dark-hard         |
|                         |                        |                   | 64 base16-gruvbox-dark-medium       |
|                         |                        |                   | 65 base16-gruvbox-dark-pale         |
|                         |                        |                   | 66 base16-gruvbox-dark-soft         |
|                         |                        |                   | 67 base16-gruvbox-light-hard        |
|                         |                        |                   | 68 base16-gruvbox-light-medium      |
|                         |                        |                   | 69 base16-gruvbox-light-soft        |
|                         |                        |                   | 70 base16-harmonic-dark             |
|                         |                        |                   | 71 base16-harmonic-light            |
|                         |                        |                   | 72 base16-heetch-light              |
|                         |                        |                   | 73 base16-heetch                    |
|                         |                        |                   | 74 base16-helios                    |
|                         |                        |                   | 75 base16-hopscotch                 |
|                         |                        |                   | 76 base16-horizon-dark              |
|                         |                        |                   | 77 base16-ia-dark                   |
|                         |                        |                   | 78 base16-ia-light                  |
|                         |                        |                   | 79 base16-icy                       |
|                         |                        |                   | 80 base16-irblack                   |
|                         |                        |                   | 81 base16-isotope                   |
|                         |                        |                   | 82 base16-macintosh                 |
|                         |                        |                   | 83 base16-marrakesh                 |
|                         |                        |                   | 84 base16-material-darker           |
|                         |                        |                   | 85 base16-material-lighter          |
|                         |                        |                   | 86 base16-material-palenight        |
|                         |                        |                   | 87 base16-material                  |
|                         |                        |                   | 88 base16-material-vivid            |
|                         |                        |                   | 89 base16-materia                   |
|                         |                        |                   | 90 base16-mellow-purple             |
|                         |                        |                   | 91 base16-mexico-light              |
|                         |                        |                   | 92 base16-mocha                     |
|                         |                        |                   | 93 base16-monokai                   |
|                         |                        |                   | 94 base16-nord                      |
|                         |                        |                   | 95 base16-oceanicnext               |
|                         |                        |                   | 96 base16-ocean                     |
|                         |                        |                   | 97 base16-onedark                   |
|                         |                        |                   | 98 base16-one-light                 |
|                         |                        |                   | 99 base16-outrun-dark               |
|                         |                        |                   | 100 base16-papercolor-dark          |
|                         |                        |                   | 101 base16-papercolor-light         |
|                         |                        |                   | 102 base16-paraiso                  |
|                         |                        |                   | 103 base16-phd                      |
|                         |                        |                   | 104 base16-pico                     |
|                         |                        |                   | 105 base16-pop                      |
|                         |                        |                   | 106 base16-porple                   |
|                         |                        |                   | 107 base16-railscasts               |
|                         |                        |                   | 108 base16-rebecca                  |
|                         |                        |                   | 109 base16-seti                     |
|                         |                        |                   | 110 base16-shapeshifter             |
|                         |                        |                   | 111 base16-snazzy                   |
|                         |                        |                   | 112 base16-solarflare               |
|                         |                        |                   | 113 base16-solarized-dark           |
|                         |                        |                   | 114 base16-solarized-light          |
|                         |                        |                   | 115 base16-spacemacs                |
|                         |                        |                   | 116 base16-summerfruit-dark         |
|                         |                        |                   | 117 base16-summerfruit-light        |
|                         |                        |                   | 118 base16-synth-midnight-dark      |
|                         |                        |                   | 119 base16-tomorrow-night           |
|                         |                        |                   | 120 base16-tomorrow                 |
|                         |                        |                   | 121 base16-tube                     |
|                         |                        |                   | 122 base16-twilight                 |
|                         |                        |                   | 123 base16-unikitty-dark            |
|                         |                        |                   | 124 base16-unikitty-light           |
|                         |                        |                   | 125 base16-woodland                 |
|                         |                        |                   | 126 base16-xcode-dusk               |
|                         |                        |                   | 127 base16-zenburn                  |
|-------------------------+------------------------+-------------------+-------------------------------------|"
  )

;; Theme switch
(cond
 ((eq emmacs-theme-package 0)
  ;; Doom themes
  (use-package doom-themes
    :ensure t
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
     )))
 ((eq emmacs-theme-package 1)
  ;; Kaolin Themes
  (use-package kaolin-themes
    :ensure t
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
  (use-package base16-theme
    :ensure t
    :config
    (cond
     ((eq emmacs-theme-number 1) (load-theme 'base16-3024 t))
     ((eq emmacs-theme-number 2) (load-theme 'base16-apathy t))
     ((eq emmacs-theme-number 3) (load-theme 'base16-ashes t))
     ((eq emmacs-theme-number 4) (load-theme 'base16-atelier-cave-light t))
     ((eq emmacs-theme-number 5) (load-theme 'base16-atelier-cave t))
     ((eq emmacs-theme-number 6) (load-theme 'base16-atelier-dune-light t))
     ((eq emmacs-theme-number 7) (load-theme 'base16-atelier-dune t))
     ((eq emmacs-theme-number 8) (load-theme 'base16-atelier-estuary-light t))
     ((eq emmacs-theme-number 9) (load-theme 'base16-atelier-estuary t))
     ((eq emmacs-theme-number 10) (load-theme 'base16-atelier-forest-light t))
     ((eq emmacs-theme-number 11) (load-theme 'base16-atelier-forest t))
     ((eq emmacs-theme-number 12) (load-theme 'base16-atelier-heath-light t))
     ((eq emmacs-theme-number 13) (load-theme 'base16-atelier-heath t))
     ((eq emmacs-theme-number 14) (load-theme 'base16-atelier-lakeside-light t))
     ((eq emmacs-theme-number 15) (load-theme 'base16-atelier-lakeside t))
     ((eq emmacs-theme-number 16) (load-theme 'base16-atelier-plateau-light t))
     ((eq emmacs-theme-number 17) (load-theme 'base16-atelier-plateau t))
     ((eq emmacs-theme-number 18) (load-theme 'base16-atelier-savanna-light t))
     ((eq emmacs-theme-number 19) (load-theme 'base16-atelier-savanna t))
     ((eq emmacs-theme-number 20) (load-theme 'base16-atelier-seaside-light t))
     ((eq emmacs-theme-number 21) (load-theme 'base16-atelier-seaside t))
     ((eq emmacs-theme-number 22) (load-theme 'base16-atelier-sulphurpool-light))
     ((eq emmacs-theme-number 23) (load-theme 'base16-atelier-sulphurpool t))
     ((eq emmacs-theme-number 24) (load-theme 'base16-atlas t))
     ((eq emmacs-theme-number 25) (load-theme 'base16-bespin t))
     ((eq emmacs-theme-number 26) (load-theme 'base16-black-metal-bathory t))
     ((eq emmacs-theme-number 27) (load-theme 'base16-black-metal-burzum t))
     ((eq emmacs-theme-number 28) (load-theme 'base16-black-metal-dark-funeral t))
     ((eq emmacs-theme-number 29) (load-theme 'base16-black-metal-gorgoroth t))
     ((eq emmacs-theme-number 30) (load-theme 'base16-black-metal-immortal t))
     ((eq emmacs-theme-number 31) (load-theme 'base16-black-metal-khold t))
     ((eq emmacs-theme-number 32) (load-theme 'base16-black-metal-marduk t))
     ((eq emmacs-theme-number 33) (load-theme 'base16-black-metal-mayhem t))
     ((eq emmacs-theme-number 34) (load-theme 'base16-black-metal-nile t))
     ((eq emmacs-theme-number 35) (load-theme 'base16-black-metal t))
     ((eq emmacs-theme-number 36) (load-theme 'base16-black-metal-venom t))
     ((eq emmacs-theme-number 37) (load-theme 'base16-brewer t))
     ((eq emmacs-theme-number 38) (load-theme 'base16-bright t))
     ((eq emmacs-theme-number 39) (load-theme 'base16-brogrammer t))
     ((eq emmacs-theme-number 40) (load-theme 'base16-brushtrees-dark t))
     ((eq emmacs-theme-number 41) (load-theme 'base16-brushtrees t))
     ((eq emmacs-theme-number 42) (load-theme 'base16-chalk t))
     ((eq emmacs-theme-number 43) (load-theme 'base16-circus t))
     ((eq emmacs-theme-number 44) (load-theme 'base16-classic-dark t))
     ((eq emmacs-theme-number 45) (load-theme 'base16-classic-light t))
     ((eq emmacs-theme-number 46) (load-theme 'base16-codeschool t))
     ((eq emmacs-theme-number 47) (load-theme 'base16-cupcake t))
     ((eq emmacs-theme-number 48) (load-theme 'base16-cupertino t))
     ((eq emmacs-theme-number 49) (load-theme 'base16-darktooth t))
     ((eq emmacs-theme-number 50) (load-theme 'base16-default-dark t))
     ((eq emmacs-theme-number 51) (load-theme 'base16-default-light t))
     ((eq emmacs-theme-number 52) (load-theme 'base16-dracula t))
     ((eq emmacs-theme-number 53) (load-theme 'base16-eighties t))
     ((eq emmacs-theme-number 54) (load-theme 'base16-embers t))
     ((eq emmacs-theme-number 55) (load-theme 'base16-flat t))
     ((eq emmacs-theme-number 56) (load-theme 'base16-fruit-soda t))
     ((eq emmacs-theme-number 57) (load-theme 'base16-github t))
     ((eq emmacs-theme-number 58) (load-theme 'base16-google-dark t))
     ((eq emmacs-theme-number 59) (load-theme 'base16-google-light t))
     ((eq emmacs-theme-number 60) (load-theme 'base16-grayscale-dark t))
     ((eq emmacs-theme-number 61) (load-theme 'base16-grayscale-light t))
     ((eq emmacs-theme-number 62) (load-theme 'base16-greenscreen t))
     ((eq emmacs-theme-number 63) (load-theme 'base16-gruvbox-dark-hard t))
     ((eq emmacs-theme-number 64) (load-theme 'base16-gruvbox-dark-medium t))
     ((eq emmacs-theme-number 65) (load-theme 'base16-gruvbox-dark-pale t))
     ((eq emmacs-theme-number 66) (load-theme 'base16-gruvbox-dark-soft t))
     ((eq emmacs-theme-number 67) (load-theme 'base16-gruvbox-light-hard t))
     ((eq emmacs-theme-number 68) (load-theme 'base16-gruvbox-light-medium t))
     ((eq emmacs-theme-number 69) (load-theme 'base16-gruvbox-light-soft t))
     ((eq emmacs-theme-number 70) (load-theme 'base16-harmonic-dark t))
     ((eq emmacs-theme-number 71) (load-theme 'base16-harmonic-light t))
     ((eq emmacs-theme-number 72) (load-theme 'base16-heetch-light t))
     ((eq emmacs-theme-number 73) (load-theme 'base16-heetch t))
     ((eq emmacs-theme-number 74) (load-theme 'base16-helios t))
     ((eq emmacs-theme-number 75) (load-theme 'base16-hopscotch t))
     ((eq emmacs-theme-number 76) (load-theme 'base16-horizon-dark t))
     ((eq emmacs-theme-number 77) (load-theme 'base16-ia-dark t))
     ((eq emmacs-theme-number 78) (load-theme 'base16-ia-light t))
     ((eq emmacs-theme-number 79) (load-theme 'base16-icy t))
     ((eq emmacs-theme-number 80) (load-theme 'base16-irblack t))
     ((eq emmacs-theme-number 81) (load-theme 'base16-isotope t))
     ((eq emmacs-theme-number 82) (load-theme 'base16-macintosh t))
     ((eq emmacs-theme-number 83) (load-theme 'base16-marrakesh t))
     ((eq emmacs-theme-number 84) (load-theme 'base16-material-darker t))
     ((eq emmacs-theme-number 85) (load-theme 'base16-material-lighter t))
     ((eq emmacs-theme-number 86) (load-theme 'base16-material-palenight t))
     ((eq emmacs-theme-number 87) (load-theme 'base16-material t))
     ((eq emmacs-theme-number 88) (load-theme 'base16-material-vivid t))
     ((eq emmacs-theme-number 89) (load-theme 'base16-materia t))
     ((eq emmacs-theme-number 90) (load-theme 'base16-mellow-purple t))
     ((eq emmacs-theme-number 91) (load-theme 'base16-mexico-light t))
     ((eq emmacs-theme-number 92) (load-theme 'base16-mocha t))
     ((eq emmacs-theme-number 93) (load-theme 'base16-monokai t))
     ((eq emmacs-theme-number 94) (load-theme 'base16-nord t))
     ((eq emmacs-theme-number 95) (load-theme 'base16-oceanicnext t))
     ((eq emmacs-theme-number 96) (load-theme 'base16-ocean t))
     ((eq emmacs-theme-number 97) (load-theme 'base16-onedark t))
     ((eq emmacs-theme-number 98) (load-theme 'base16-one-light t))
     ((eq emmacs-theme-number 99) (load-theme 'base16-outrun-dark t))
     ((eq emmacs-theme-number 100) (load-theme 'base16-papercolor-dark t))
     ((eq emmacs-theme-number 101) (load-theme 'base16-papercolor-light t))
     ((eq emmacs-theme-number 102) (load-theme 'base16-paraiso t))
     ((eq emmacs-theme-number 103) (load-theme 'base16-phd t))
     ((eq emmacs-theme-number 104) (load-theme 'base16-pico t))
     ((eq emmacs-theme-number 105) (load-theme 'base16-pop t))
     ((eq emmacs-theme-number 106) (load-theme 'base16-porple t))
     ((eq emmacs-theme-number 107) (load-theme 'base16-railscasts t))
     ((eq emmacs-theme-number 108) (load-theme 'base16-rebecca t))
     ((eq emmacs-theme-number 109) (load-theme 'base16-seti t))
     ((eq emmacs-theme-number 110) (load-theme 'base16-shapeshifter t))
     ((eq emmacs-theme-number 111) (load-theme 'base16-snazzy t))
     ((eq emmacs-theme-number 112) (load-theme 'base16-solarflare t))
     ((eq emmacs-theme-number 113) (load-theme 'base16-solarized-dark t))
     ((eq emmacs-theme-number 114) (load-theme 'base16-solarized-light t))
     ((eq emmacs-theme-number 115) (load-theme 'base16-spacemacs t))
     ((eq emmacs-theme-number 116) (load-theme 'base16-summerfruit-dark t))
     ((eq emmacs-theme-number 117) (load-theme 'base16-summerfruit-light t))
     ((eq emmacs-theme-number 118) (load-theme 'base16-synth-midnight-dark t))
     ((eq emmacs-theme-number 119) (load-theme 'base16-theme-autoloads.el t))
     ((eq emmacs-theme-number 120) (load-theme 'base16 t))
     ((eq emmacs-theme-number 121) (load-theme 'base16-theme-pkg.el t))
     ((eq emmacs-theme-number 119) (load-theme 'base16-tomorrow-night t))
     ((eq emmacs-theme-number 120) (load-theme 'base16-tomorrow t))
     ((eq emmacs-theme-number 121) (load-theme 'base16-tube t))
     ((eq emmacs-theme-number 122) (load-theme 'base16-twilight t))
     ((eq emmacs-theme-number 123) (load-theme 'base16-unikitty-dark t))
     ((eq emmacs-theme-number 124) (load-theme 'base16-unikitty-light t))
     ((eq emmacs-theme-number 125) (load-theme 'base16-woodland t))
     ((eq emmacs-theme-number 126) (load-theme 'base16-xcode-dusk t))
     ((eq emmacs-theme-number 127) (load-theme 'base16-zenburn t))
     )
    (set-face-attribute 'widget-button nil :underline nil)))
 ((eq emmacs-theme-package 5)
  (use-package zenburn-theme
    :config
    (load-theme 'zenburn t))))

(cond ((eq emmacs-treemacs-theme 0)
       (use-package doom-themes
	 :ensure t
	 :config
	 (doom-themes-treemacs-config)))
      ((eq emmacs-treemacs-theme 1)
       ;; Kaolin Themes
       (use-package kaolin-themes
	 :ensure t
	 :config
	 (kaolin-treemacs-theme))))
      
 (provide '1_emmacs_themes)
;;; 1_emmacs_themes.el ends here
