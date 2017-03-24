;;; init.lisp --- StumpWM Init File
(in-package :stumpwm)
;(setq *debug-level* 9)
;(set-font "--Inconsolata-----30-------")
(set-font "Inconsolata")
;(if (not (head-mode-line (current-head)))
;	(toggle-mode-line (current-screen) (current-head)))
;(load-module "ttf-fonts")
;(set-font (make-instance 'xft:font :family "Ubuntu Mono" :subfamily "Regular" :size 19))
;(set-font "-misc-fixed-medium-r-semicondensed-*-*-*-*-*-*-*-*-*")
;(set-font "-*-*-*-*-Inconsolata-*-*-*-*-*-*-*-*-*")
(toggle-mode-line (current-screen) (current-head))
(defcommand emacs () ()
	"run-or-raise emacs"
	(run-or-raise "emacsclient -ca emacs" '(:class "Emacs")))
(define-key *root-map* (kbd "C-e") "emacs")
(defcommand firefox () ()
	"run-or-raise firefox"
	(run-or-raise "firefox" '(:class "Firefox")))
(define-key *root-map* (kbd "C-f") "firefox")
(defcommand mlterm () ()
	"run-or-raise mlterm"
	(run-or-raise "mlterm" '(:class "mlterm")))
(define-key *root-map* (kbd "C-t") "mlterm")
