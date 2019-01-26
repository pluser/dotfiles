;;; -*- mode: Emacs-Lisp -*- ;;;
;;; customize.el --- A file for emacs customize mechanisme.

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-jump-face-foreground ((t (:foreground "yellow" :underline nil))))
 '(company-preview-common ((t (:inherit company-preview :foreground "orange"))))
 '(compilation-error ((t (:foreground "yellow"))))
 '(error ((t (:foreground "yellow" :weight bold))))
 '(flycheck-error ((t (:underline (:color "chartreuse" :style wave)))))
 '(flycheck-info ((t (:underline (:color "deep sky blue" :style wave)))))
 '(flycheck-warning ((t (:underline (:color "medium spring green" :style wave)))))
 '(hl-line ((t (:inherit highlight :background "#383a46"))))
 '(mode-line ((t (:background "gold" :foreground "black" :box nil))))
 '(show-paren-match ((t (:background "dark slate blue"))))
 '(term-color-black ((t (:background "gray5" :foreground "gray5"))))
 '(term-color-blue ((t (:background "slate blue" :foreground "slate blue"))))
 '(term-color-cyan ((t (:background "deep sky blue" :foreground "deep sky blue"))))
 '(term-color-green ((t (:background "SpringGreen3" :foreground "SpringGreen3"))))
 '(term-color-magenta ((t (:background "maroon1" :foreground "maroon1"))))
 '(term-color-red ((t (:background "#ee5533" :foreground "#ee5533"))))
 '(term-color-white ((t (:background "gainsboro" :foreground "gainsboro"))))
 '(term-color-yellow ((t (:background "yellow2" :foreground "yellow2"))))
 '(whitespace-tab ((t (:foreground "darkgray")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(company-lsp ccls ox-pandoc dart-mode lsp-ui swift-mode cquery evil magit yasnippet projectile company helm flycheck origami which-key web-mode markdown-mode idomenu ddskk))))
