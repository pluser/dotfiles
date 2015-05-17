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
 '(term-color-black ((t (:background "gray5" :foreground "gray5"))))
 '(term-color-blue ((t (:background "slate blue" :foreground "slate blue"))))
 '(term-color-cyan ((t (:background "deep sky blue" :foreground "deep sky blue"))))
 '(term-color-green ((t (:background "SpringGreen3" :foreground "SpringGreen3"))))
 '(term-color-magenta ((t (:background "maroon1" :foreground "maroon1"))))
 '(term-color-red ((t (:background "#ee5533" :foreground "#ee5533"))))
 '(term-color-white ((t (:background "gainsboro" :foreground "gainsboro"))))
 '(term-color-yellow ((t (:background "yellow2" :foreground "yellow2")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style
   (quote
	((c++-mode . "stroustrup")
	 (java-mode . "java")
	 (awk-mode . "awk")
	 (other . "gnu"))))
 '(c-tab-always-indent nil)
 '(make-backup-files nil)
 '(package-selected-packages
   (quote
	(projectile company-web company-jedi pyvenv jinja2-mode python-mode company helm web-mode sunrise-commander php-mode markdown-mode magit idomenu flycheck evil elpy dired+ dic-lookup-w3m ddskk company-c-headers ace-isearch)))
 '(tab-width 4))
