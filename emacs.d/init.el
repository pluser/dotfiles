;;; This is Emacs Setting file.

;;; Local variable
(when (< emacs-major-version 23)
  (defvar user-emacs-directory (file-name-directory (or buffer-file-name load-file-name))))

;;; Set load-path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "elisps/"))
(when (< emacs-major-version 24)	; for no 'package env.
 (add-to-list 'load-path (concat user-emacs-directory "packages/"))
 (let ((default-directory (concat user-emacs-directory "packages/")))
   (normal-top-level-add-subdirs-to-load-path)))

;;; Package Managing
(when (require 'package nil t)
  (add-to-list 'package-archives '("melpa". "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade". "http://marmalade-repo.org/packages/") t)
  (setq package-user-dir(concat user-emacs-directory "packages/"))
  (package-initialize))

;;; Font Setting
(when (equal window-system 'x)
  (load-file (concat user-emacs-directory "fonts.el"))
  (set-frame-font "Inconsolata"))

;;; User Interface Setting
(if (< emacs-major-version 24) (tool-bar-mode nil) (tool-bar-mode 0)
(menu-bar-mode nil))

;;; Color Setting
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))
;; Extension: color-theme
(when (require 'color-theme nil t)	; Extension: color-theme
  (color-theme-initialize)
  (color-theme-deep-blue))

;;; Communication with external program
(setq diff-switches "-u")

;;; Buffer behavior
(when (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;;; Code ornament / Visual
(global-font-lock-mode t)
(show-paren-mode t)
(setq show-paren-style 'mixed)
(transient-mark-mode t)
(column-number-mode t)
;; Display line number
;(when (require 'linum) (global-linum-mode t))
(setq scroll-conservatively 1)

;;; Code behavior
(delete-selection-mode t)
(setq require-final-newline t)

;;; Completion
(when (require 'auto-complete nil t)		; Extension: auto-complete
  (add-to-list 'ac-dictionary-directories (concat user-emacs-directory "acdict/"))
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
