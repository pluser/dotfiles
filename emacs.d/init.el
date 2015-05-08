;;; -*- mode: Emacs-Lisp; coding: utf-8 -*- ;;;
;;; init.el --- A emacs setting file

;;; Local variable
(when (< emacs-major-version 23)
  (defvar user-emacs-directory (file-name-directory (or buffer-file-name load-file-name))))

;;; Set load-path
;(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "elisps/"))
(add-to-list 'load-path (concat user-emacs-directory "monkeys/"))
(when (< emacs-major-version 24)	; if emacs didn't have package system
 (add-to-list 'load-path (concat user-emacs-directory "packages/"))
 (let ((default-directory (concat user-emacs-directory "packages/")))
   (normal-top-level-add-subdirs-to-load-path)))
(add-to-list 'load-path (concat user-emacs-directory "local/"))
(let ((default-directory (concat user-emacs-directory "local/")))
 (normal-top-level-add-subdirs-to-load-path))

;;; Package Managing
(when (require 'package nil t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("sc" . "http://joseito.republika.pl/sunrise-commander/") t)
  (add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
  (setq package-user-dir (concat user-emacs-directory "packages/"))
  (package-initialize))
(defmacro package-config (package &rest body)
  "Load and set package settings."
  (declare (indent defun))
  `(eval-after-load ,package (lambda () ,@body)))

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

;;; User Interface Setting
(setq transient-mark-mode nil)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(if (< emacs-major-version 24) (tool-bar-mode nil) (tool-bar-mode 0))
(if (< emacs-major-version 24) (menu-bar-mode nil) (menu-bar-mode 0))

;;; Color / Theme Setting
(if (< emacs-major-version 24)
    (when (require 'color-theme nil t)	; Extension: color-theme
      (color-theme-initialize)
      (color-theme-deep-blue))
  (load-theme 'deeper-blue))

;;; File Opener
(when (require 'filecache nil t)	; Extension: filecache
  (file-cache-add-file (concat user-emacs-top-directory "init.el")))

;;; Diff Setting
(setq diff-switches "-u")

;;; Code ornament / Visual
(show-paren-mode t)
(setq show-paren-style 'mixed)
(setq scroll-conservatively 0)
(line-number-mode t)
(column-number-mode t)

;;; Code styling
(setq require-final-newline nil)

;;; Completion
(setq completion-ignore-case t)
;(setq read-file-name-completion-ignore-case t)
;(partial-completion-mode t)
(icomplete-mode t)

;;; History
(setq history-length 10000)
(savehist-mode t)
(setq recentf-max-saved-items 10000)

;;; Compression
(auto-compression-mode t)

;;; Define global keymap
(define-key global-map (kbd "C-h") 'backward-delete-char-untabify)
(define-key global-map (kbd "<muhenkan>") 'pop-to-mark-command)
(define-key global-map (kbd "<menu>") 'repeat)

;;; Load Private confidential file
(load-file (concat user-emacs-top-directory "private/email.el"))

;;; Helm Settings
(package-config 'helm-mode		; Extension: helm
  (require 'helm-config)
  (define-key global-map (kbd "C-x b") 'helm-buffers-list)
  (define-key global-map (kbd "C-x f") 'helm-find-files)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "C-z") 'helm-select-action)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "TAB") 'helm-select-action))
(add-hook 'after-init-hook 'helm-mode)

;;; Evil Settings
(package-config 'evil		; Extension: evil
  (dolist (state '(normal motion visual))
    (define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "h") 'evil-backward-char)
    (define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "t") 'evil-next-line)
    (define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "n") 'evil-previous-line)
    (define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "s") 'evil-forward-char)
	(define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "j") 'evil-find-char-to)
	(define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "J") 'evil-find-char-to-backward
))
  (define-key evil-insert-state-map (kbd "M-SPC") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-e") nil)
  (setq evil-move-cursor-back nil)
  (setq evil-default-state 'emacs)
  (setq evil-echo-state nil)
  (setq evil-insert-state-cursor nil)
  (eval-after-load 'help-mode '(evil-make-overriding-map help-mode-map)))
(add-hook 'after-init-hook 'evil-mode)

;;; Company Settings
(package-config 'company		; Extension: company
  (define-key global-map (kbd "<henkan>") 'company-complete)
  (setq company-idle-delay nil))
(add-hook 'after-init-hook 'global-company-mode)

;;; Magit Settings
(package-config 'magit		; Extension: magit
  (define-key global-map (kbd "<f12>") 'magit-status))

;;; Auto-Complete Settings
(package-config 'auto-complete		; Extension: auto-complete
;  (add-to-list 'ac-dictionary-directories (concat user-emacs-directory "acdict/"))
  (require 'auto-complete-config)
  (ac-config-default)
  (setq ac-auto-show-menu nil)
  (setq ac-use-quick-help nil)
  (define-key ac-mode-map "\M-/" 'auto-complete))

;;; Web-mode Settings
(package-config 'web-mode		; Extension: web-mode
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

;;; Python-mode Settings
(package-config 'python	; Extension: python-mode
  (add-hook 'python-mode-hook
			(lambda ()
			  (setq indent-tabs-mode t)
			  (setq python-indent-offset 4)
			  (setq tab-width 4))))

;;; Elpy Settings
(package-config 'elpy
  (setq elpy-modules (delete 'elpy-module-highlight-indentation elpy-modules)))

;;; Undo-Tree Settings
(package-config 'undo-tree		; Extension: undo-tree
  (setq undo-tree-mode-lighter " Ut"))

;;; Skk Settings
(setq skk-user-directory (concat user-emacs-directory "ddskk/"))
(package-config 'skk		; Extension: SKK
  (setq skk-user-directory (concat user-emacs-directory "ddskk/"))
  (setq default-input-method "japanese-skk")
  (define-key global-map "\C-x\C-j" 'skk-mode)
  (setq skk-henkan-show-candidates-keys (list ?a ?o ?e ?u ?h ?t ?n ?s))
  (setq skk-indicator-use-cursor-color nil)
  (setq skk-kakutei-key (kbd "C-t"))
  (setq skk-background-mode 'dark) ; For DDSKK 15.1 Hot-Fix
  (require 'skk-hint nil t)
  (when (require 'skk-search-web nil t)
;   (add-to-list 'skk-search-prog-list '(skk-search-web 'skk-google-cgi-api-for-japanese-input) t)
    (add-to-list 'skk-search-prog-list '(skk-search-web 'skk-google-suggest) t)))

;;; w3m Settings
(package-config 'w3m-filter		; Extension: w3m
; (defun w3m-filter-alc-lay (url) (w3m-filter-delete-regions url "<!-- interest_match_relevant_zone_start -->" "<!-- ▲ 英辞郎ヘッダ ▲ -->"))
  (defun w3m-filter-alc-lay (url) (w3m-filter-delete-regions url "<!-- interest_match_relevant_zone_start -->" "<!-- ▼ 検索結果本体 ▼ -->"))
  (add-to-list 'w3m-filter-configuration '(t "Suck!" "\\`http://eow\\.alc\\.co\\.jp/search" w3m-filter-alc-lay)))


;;; Tramp Settings
;; A workaround for Tramp for text corruption.
;(setq tramp-remote-process-environment (quote ("HISTFILE=$HOME/.tramp_history" "HISTSIZE=1" "LC_MESSAGE=C" "TERM=dumb" "EMACS=t" "INSIDE_EMACS='24.3.1,tramp:2.2.6-24.3'" "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH=" "PAGER=\"\"" "autocorrect=" "correct=")))

;;; Terminal Emulator Settings
;(when (require 'term+ nil t)
;  (require 'xterm-256color nil t))

;;; Uniquify Settings
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;;; Dired Settings
(setq dired-listing-switches "-alh")
(setq dired-dwim-target t)

;;; Ediff Settiings
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; Customize Settings
(setq custom-file (concat user-emacs-top-directory "customize.el"))
(load custom-file)
