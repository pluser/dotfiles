;;; This is Emacs Setting file.

;;; Local variable
(setq conf-dir (file-name-directory (or (buffer-file-name) load-file-name)))

;;; Set load-path
(add-to-list 'load-path conf-dir)
(add-to-list 'load-path (concat conf-dir "elisps/"))
(add-to-list 'load-path (concat conf-dir "packages/"))
(let ((default-directory (concat conf-dir "packages/")))
  (normal-top-level-add-subdirs-to-load-path))

;;; Define keymap
(define-key global-map "\C-h" 'delete-backward-char)
(define-key global-map "\C-x\C-h" 'help-command)
(define-key global-map "\M-/" 'dabbrev-expand)

;;; Font Setting
(when (equal window-system 'x)
  (load-file (concat conf-dir "fonts.el"))
  (set-frame-font "Inconsolata"))

;;; User Interface Setting
(tool-bar-mode nil)
;(menu-bar-mode nil)

;;; Color Setting
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))
(when (require 'color-theme)
  (color-theme-initialize)
  (color-theme-deep-blue))

;;; Communication with external program
(setq diff-switches "-u")

;;; Buffer behavior
(when (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;;; Code ornament
(global-font-lock-mode t)
(show-paren-mode t)
(setq show-paren-style 'mixed)
(transient-mark-mode t)
(column-number-mode t)
;; Display line number
;(when (require 'linum) (global-linum-mode t))

;;; Code behavior
(delete-selection-mode t)
(setq require-final-newline t)

;;; Completion
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;; ex. p-b -> print-buffer
(partial-completion-mode t)
;; display ordinary
(icomplete-mode t)

;;; History
(setq history-length 10000)
(savehist-mode t)
(setq recentf-max-saved-items 10000)

;;; Compression
(auto-compression-mode t)
