;;; pluser-deeper-blue-theme.el --- An custom theme for emacs
;;; Commentary:
;;; This theme is just an 'overlay' of the deeper-blue theme.
;;; Use this theme with built-in deeper-blue theme.
;;; Code:

(deftheme pluser-deeper-blue
  "Created 2019-01-27.")

(custom-theme-set-faces
 'pluser-deeper-blue
 '(ace-jump-face-foreground ((t (:foreground "yellow" :underline nil))))
 '(company-preview-common ((t (:inherit company-preview :foreground "orange"))))
 '(warning ((t (:foreground "orchid"))))
 '(error ((t (:foreground "violet" :weight bold))))
 '(compilation-error ((t (:foreground "yellow"))))
 '(flycheck-error ((t (:underline (:color "chartreuse" :style wave)))))
 '(flycheck-info ((t (:underline (:color "deep sky blue" :style wave)))))
 '(flycheck-warning ((t (:underline (:color "medium spring green" :style wave)))))
 '(hl-line ((t (:inherit highlight :background "#383a46"))))
 '(mode-line ((t (:background "gold" :foreground "black" :box nil))))
 '(telephone-line-projectile ((t (:foreground "dark slate gray"))))
 '(show-paren-match ((t (:background "dark slate blue"))))
 '(term-color-black ((t (:background "gray5" :foreground "gray5"))))
 '(term-color-blue ((t (:background "slate blue" :foreground "slate blue"))))
 '(term-color-cyan ((t (:background "deep sky blue" :foreground "deep sky blue"))))
 '(term-color-green ((t (:background "SpringGreen3" :foreground "SpringGreen3"))))
 '(term-color-magenta ((t (:background "maroon1" :foreground "maroon1"))))
 '(term-color-red ((t (:background "#ee5533" :foreground "#ee5533"))))
 '(term-color-white ((t (:background "gainsboro" :foreground "gainsboro"))))
 '(term-color-yellow ((t (:background "yellow2" :foreground "yellow2"))))
 '(whitespace-tab ((t (:foreground "darkgray"))))
 '(helm-selection ((t (:background "dark slate blue"))))
 '(helm-source-header ((t (:background unspecified :height unspecified)))))
(custom-theme-set-variables
 'pluser-deeper-blue
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(telephone-line-primary-left-separator telephone-line-identity-left)
 '(telephone-line-primary-right-separator telephone-line-identity-right)
 '(telephone-line-secondary-left-separator telephone-line-identity-hollow-left)
 '(telephone-line-secondary-right-separator telephone-line-identity-hollow-right))

(provide-theme 'pluser-deeper-blue)

;;; pluser-deeper-blue-theme.el ends here
