;;; -*- mode: Emacs-Lisp; coding: -*- ;;;
;;; init.el --- A emacs setting file

;;; Local variable
(when (< emacs-major-version 23)
  (defvar user-emacs-directory (file-name-directory (or buffer-file-name load-file-name))))

;;; Set load-path
;(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "elisps/"))
(add-to-list 'load-path (concat user-emacs-directory "monkeys/"))
(when (< emacs-major-version 24)	; for no 'package env.
 (add-to-list 'load-path (concat user-emacs-directory "packages/"))
 (let ((default-directory (concat user-emacs-directory "packages/")))
   (normal-top-level-add-subdirs-to-load-path)))
(add-to-list 'load-path (concat user-emacs-directory "local/"))
(let ((default-directory (concat user-emacs-directory "local/")))
 (normal-top-level-add-subdirs-to-load-path))

;;; Package Managing
(when (require 'package nil t)
  (add-to-list 'package-archives '("melpa". "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade". "http://marmalade-repo.org/packages/") t)
  (setq package-user-dir (concat user-emacs-directory "packages/"))
  (package-initialize))

;;; Font Setting
(when (and (display-graphic-p) (file-readable-p (concat user-emacs-directory "fonts.el")))
  (load-file (concat user-emacs-directory "fonts.el"))
  (when (< emacs-major-version 24)
    (set-face-font 'default "fontset-Aoshima")
    (set-frame-font "fontset-Aoshima"))
  (add-to-list 'default-frame-alist '(font . "fontset-Aoshima")))

;;; Custom Patch Loading
(when (file-readable-p (concat user-emacs-directory "monkey.el"))
  (load-file (concat user-emacs-directory "monkey.el")))

;;; A Hack for Cache
(setq user-emacs-top-directory user-emacs-directory)
(setq user-emacs-directory (concat user-emacs-directory "cache/"))

;;; Language Setting
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(when (require 'skk-autoloads nil t)
  (setq skk-user-directory (concat user-emacs-directory "ddskk/"))
  (setq default-input-method "japanese-skk")
  (define-key global-map "\C-x\C-j" 'skk-mode)
  (setq skk-henkan-show-candidates-keys (list ?a ?o ?e ?u ?h ?t ?n ?s))
  (setq skk-indicator-use-cursor-color nil)
  (setq skk-kakutei-key (kbd "C-t"))
  (setq skk-background-mode 'dark) ; For DDSKK 15.1 Hot-Fix
  (require 'skk-hint nil t)
  (when (require 'skk-search-web nil t)
;    (add-to-list 'skk-search-prog-list '(skk-search-web 'skk-google-cgi-api-for-japanese-input) t)
    (add-to-list 'skk-search-prog-list '(skk-search-web 'skk-google-suggest) t)))

;;; User Interface Setting
;(setq visible-bell t)
(setq ring-bell-function 'ignore)
(when (require 'helm nil t)		; Extension: helm
  (require 'helm-config))
(if (< emacs-major-version 24) (tool-bar-mode nil) (tool-bar-mode 0))
(if (< emacs-major-version 24) (menu-bar-mode nil) (menu-bar-mode 0))
(when (require 'evil nil t)		; Extension: evil
  (defun evil-execute-in-normal-state-at-emacs-state ()
    "Execute the next command in Normal state at Emacs state."
    (interactive)
    (evil-delay '(not (memq this-command
			    '(evil-execute-in-normal-state-at-emacs-state
			      evil-use-register
			      digit-argument
			      negative-argument
			      universal-argument
			      universal-argument-minus
			      universal-argument-more
			      universal-argument-other-key)))
		`(progn
		   (evil-emacs-state)
		   (setq evil-move-cursor-back ',evil-move-cursor-back))
		'post-command-hook)
    (setq evil-move-cursor-back nil)
    (evil-normal-state)
    (evil-echo "Command?"))
;  (global-set-key (kbd "<henkan>") 'evil-execute-in-normal-state-at-emacs-state)
  (define-key evil-normal-state-map (kbd "<henkan>") 'evil-emacs-state)
  (define-key evil-emacs-state-map (kbd "<henkan>") 'evil-normal-state)
  (setq evil-default-state 'emacs))

;;; Color / Theme Setting
(if (< emacs-major-version 24)
    (when (require 'color-theme nil t)	; Extension: color-theme
      (color-theme-initialize)
      (color-theme-deep-blue))
  (load-theme 'deeper-blue))

;;; Communication with external program
(setq diff-switches "-u")

;;; Buffer behavior
(when (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;;; Code ornament / Visual
(global-font-lock-mode t)
(show-paren-mode t)
(setq show-paren-style 'mixed)
;(transient-mark-mode -1)
(setq scroll-conservatively 0)
(line-number-mode t)
(column-number-mode t)
;; Display line number
;(when (require 'linum) (global-linum-mode t))

;;; Terminal Emulator
;(when (require 'term+ nil t)
;  (require 'xterm-256color))

;;; Behavior
;(delete-selection-mode nil)
(setq require-final-newline nil)

;;; Completion
(when (require 'auto-complete nil t)		; Extension: auto-complete
;  (add-to-list 'ac-dictionary-directories (concat user-emacs-directory "acdict/"))
  (require 'auto-complete-config)
  (ac-config-default)
  (define-key ac-mode-map "\M-/" 'auto-complete))
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;; ex. p-b -> print-buffer
;(partial-completion-mode t)
;; display ordinary
(icomplete-mode t)

;;; History
(setq history-length 10000)
(savehist-mode t)
(setq recentf-max-saved-items 10000)

;;; Compression
(auto-compression-mode t)

;;; Define global keymap
(define-key global-map "\C-h" 'delete-backward-char)
(define-key global-map "\C-x\C-h" 'help-command)
;(define-key global-map "\M-/" 'dabbrev-expand)

;;; Load Private confidential file
(load-file (concat user-emacs-top-directory "private/email.el"))

;;; A workaround for Tramp for text corruption.
(setq tramp-remote-process-environment (quote ("HISTFILE=$HOME/.tramp_history" "HISTSIZE=1" "LC_MESSAGE=C" "TERM=dumb" "EMACS=t" "INSIDE_EMACS='24.3.1,tramp:2.2.6-24.3'" "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH=" "PAGER=\"\"" "autocorrect=" "correct=")))

;;; Dired Settings
(setq dired-listing-switches "-alh")

;;; Ediff Settiings
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; Customize Settings
(setq custom-file (concat user-emacs-top-directory "customize.el"))
(load custom-file)

