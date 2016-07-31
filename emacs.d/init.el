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

;;; External file loader
(defun ext-config (filename &optional ignore-missing)
  "Load and eval the file, if it exist.
If the file does not exist, issue an warning message but error.
If IGNORE-MISSING is non-nil, the warning message will be suppress even if the file does not exist."
  (let ((filepath (if (boundp 'user-emacs-top-directory)
					  (concat user-emacs-top-directory filename)
					(concat user-emacs-directory filename))))
	(if (file-readable-p filepath)
		(load-file filepath)
	  (unless ignore-missing
		(warn "Failed to load file '%s' by '%s'."
			  filename
			  (file-name-nondirectory (or buffer-file-name load-file-name)))))))

;;; Package Managing
(defmacro package-config (package &rest body)
  "Load and set package settings."
  (declare (indent defun))
  (if (fboundp 'with-eval-after-load)
	  `(with-eval-after-load ,package ,@body)	; with-eval- is more better.
	  `(eval-after-load ,package (lambda () ,@body))))

(defun package-invoke (package-initiater &optional hook)
  "Set the package up in startup sequence.
If HOOK is non-nil, hang invoking package into HOOK instead of startup sequence."
  (if (fboundp package-initiater)
	  (add-hook (or hook 'emacs-startup-hook) package-initiater)
	(unless (require package-initiater nil t)
	  (warn "Failed to start up a package '%s'." package-initiater))))

(package-config 'package
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  ;;(add-to-list 'package-archives '("sc" . "http://joseito.republika.pl/sunrise-commander/") t)
  ;;(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
  (setq package-user-dir (concat user-emacs-directory "packages/"))
  (package-initialize))
(package-invoke 'package)

;;; Font Setting
(when (and (display-graphic-p) (file-readable-p (concat user-emacs-directory "fonts.el")))
  (ext-config "fonts.el")
  (when (< emacs-major-version 24)
    (set-face-font 'default "fontset-turu")
    (set-frame-font "fontset-turu"))
  (add-to-list 'default-frame-alist '(font . "fontset-turu")))

;;; Custom Patch Loading
(ext-config "monkey.el" t)

;;; A Hack for Cache
(setq user-emacs-top-directory user-emacs-directory)
(setq user-emacs-directory (concat user-emacs-directory "cache/"))

;;; Language Setting
;;;  If you want to know charset priority, (print (charset-priority-list))
;; (set-language-environment 'Japanese)	; set for using japanese ONLY
(prefer-coding-system 'utf-8)
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs))
;(reset-language-environment)
;;(set-charset-priority 'unicode)
(package-config 'uim
  (setq default-input-method 'japanese-google-cgiapi-jp-uim))
;;(package-invoke 'uim-leim)
(when (eq window-system 'mac)
  (mac-auto-ascii-mode))

;;; User Interface Setting
(setq transient-mark-mode nil)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq use-dialog-box nil)
(if (< emacs-major-version 24) (tool-bar-mode nil) (tool-bar-mode 0))
(if (< emacs-major-version 24) (menu-bar-mode nil) (menu-bar-mode 0))

;;; Color / Theme Setting
(if (fboundp 'load-theme)
	(load-theme 'deeper-blue)
  (package-config 'color-theme		; Extension: color-theme
	(color-theme-initialize)
	(color-theme-deep-blue))
  (package-invoke 'color-theme))

;;; File Opener
(package-config 'filecache
  (file-cache-add-file (concat user-emacs-top-directory "init.el")))
(package-invoke 'filecache)

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
(setq mode-require-final-newline nil)

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
(ext-config "private/email.el" t)

;;; Suppress warning
(setq ad-redefinition-action 'accept)

;;; Org-mode Settings
(package-config 'org-faces
  (defun advice-org-override-variable ()
	(setq org-plain-link-re
		  (concat "\\<" (regexp-opt org-link-types t) ":"
				  ;;(org-re "\\(//\\|/\\)[a-zA-Z0-9.:_---?/%\t]*\\>"))))
				  ;;(org-re "\\(//\\|/\\)[^][<>\t*^`\{}]*\\>"))))
				  (org-re "//?\\([a-zA-Z0-9]\\.\\|[a-zA-Z0-9---@:]\\)*/?\
\\(%[a-fA-F0-9][a-fA-F0-9]\\|[a-zA-Z0-9---._~!$&'()*+,;=:@/?]\\)*#?\
\\(%[a-fA-F0-9][a-fA-F0-9]\\|[a-zA-Z0-9---._~!$&'()*+,;=:@/?]\\)*"))))
  (advice-add 'org-make-link-regexps :after 'advice-org-override-variable))

;;; Helm Settings
(package-config 'helm-mode		; Extension: helm
  (require 'helm-config)
  (define-key global-map (kbd "C-x b") 'helm-buffers-list)
  (define-key global-map (kbd "C-x f") 'helm-find-files)
  (define-key global-map (kbd "M-x") 'helm-M-x)
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (setq helm-autoresize-max-height 80)
  (helm-autoresize-mode t))
(package-invoke 'helm-mode)

;;; Evil Settings
(package-config 'evil		; Extension: evil
  (dolist (state '(normal motion visual))
    (define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "h") 'evil-backward-char)
    (define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "t") 'evil-next-line)
    (define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "n") 'evil-previous-line)
    (define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "s") 'evil-forward-char)
	(define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "j") 'evil-find-char-to)
	(define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "J") 'evil-find-char-to-backward))
  (define-key evil-insert-state-map (kbd "M-SPC") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-e") nil)
  (setq evil-move-cursor-back nil)
  ;;(setq evil-default-state 'emacs)
  (setq evil-echo-state nil)
  (setq evil-insert-state-cursor nil)
  (eval-after-load 'help-mode '(evil-make-overriding-map help-mode-map)))
;(add-hook 'emacs-startup-hook 'evil-mode)

;;; Company Settings
(package-config 'company		; Extension: company
  (define-key global-map (kbd "<henkan>") 'company-complete)
  (setq company-idle-delay nil)
  (setq company-selection-wrap-around t)
  (package-config 'company-math		; Extension: company-math
	(add-to-list 'company-backends 'company-math-symbols-unicode)))
(package-invoke 'global-company-mode)

;;; Magit Settings
(package-config 'magit		; Extension: magit
  (define-key global-map (kbd "<f12>") 'magit-status)
  (setq magit-last-seen-setup-instruction "1.4.0"))

;;; Yasnippet Settings
(package-config 'yasnippet		; Extension: yasnippet
  (setq yas-snippet-dirs (list (concat user-emacs-top-directory "yasnippets/"))))

;;; Projectile Settings
(package-config 'projectile		; Extension: projectile
  (setq projectile-mode-line (format " Proj[%s]" (projectile-project-name))))
(package-invoke 'projectile-mode)

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
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

;;; Python-mode Settings
(package-config 'python		; Extension: python-mode
  (defun my/setting-python-mode ()
	(setq indent-tabs-mode t)
	(setq python-indent-offset 4)
	(setq tab-width 4))
  (defun my/enable-company-jedi ()
	(add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/setting-python-mode)
  (add-hook 'python-mode-hook 'my/enable-company-jedi))

(package-config 'jinja2-mode
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . jinja2-mode)))

;;; Elpy Settings
(package-config 'elpy		; Extension: elpy
  (setq elpy-modules (delete 'elpy-module-highlight-indentation elpy-modules)))

;;; Undo-Tree Settings
(package-config 'undo-tree		; Extension: undo-tree
  (setq undo-tree-mode-lighter " Ut"))

;;; Skk Settings
(setq skk-user-directory (concat user-emacs-directory "ddskk/"))
(package-config 'skk		; Extension: SKK
  ;;  (define-key global-map "\C-x\C-j" 'skk-mode)
  (setq skk-henkan-show-candidates-keys (list ?a ?o ?e ?u ?h ?t ?n ?s))
  (setq skk-indicator-use-cursor-color nil)
  (setq skk-kakutei-key (kbd "C-t"))
  (setq skk-background-mode 'dark) ; For DDSKK 15.1 Hot-Fix
  (require 'skk-hint nil t)
  (when (require 'skk-search-web nil t)
;   (add-to-list 'skk-search-prog-list '(skk-search-web 'skk-google-cgi-api-for-japanese-input) t)
    (add-to-list 'skk-search-prog-list '(skk-search-web 'skk-google-suggest) t)))
(package-invoke 'skk-preload 'text-mode-hook)

;;; w3m Settings
(package-config 'w3m-filter		; Extension: w3m
; (defun w3m-filter-alc-lay (url) (w3m-filter-delete-regions url "<!-- interest_match_relevant_zone_start -->" "<!-- ▲ 英辞郎ヘッダ ▲ -->"))
  (defun w3m-filter-alc-lay (url) (w3m-filter-delete-regions url "<!-- interest_match_relevant_zone_start -->" "<!-- ▼ 検索結果本体 ▼ -->"))
  (add-to-list 'w3m-filter-configuration '(t "Suck!" "\\`http://eow\\.alc\\.co\\.jp/search" w3m-filter-alc-lay)))

;;; Whitespace Settings
(package-config 'whitespace		; Extension: whitespace
  (setq whitespace-style '(face tabs trailing space-before-tab empty tab-mark)))
(package-invoke 'whitespace-mode 'find-file-hook)
(defface dspace-emphasis '((t :background "red")) "Used for dspace emphasis")
(defun emphasis-dspace ()
  (font-lock-add-keywords nil '(("　" . dspace-emphasis))))
(font-lock-add-keywords 'lisp-mode '(("b" . (0 highlight t t))))

;;; Xah-math-input-mode Settings
(package-config 'xah-math-input
  (puthash "wb" "◦" xah-math-input-abrvs))

;;; Uniquify Settings
(package-config 'uniquify
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))
(package-invoke 'uniquify)

;;; Tramp Settings
;; A workaround for Tramp for text corruption.
;(setq tramp-remote-process-environment (quote ("HISTFILE=$HOME/.tramp_history" "HISTSIZE=1" "LC_MESSAGE=C" "TERM=dumb" "EMACS=t" "INSIDE_EMACS='24.3.1,tramp:2.2.6-24.3'" "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH=" "PAGER=\"\"" "autocorrect=" "correct=")))

;;; Terminal Emulator Settings
;(when (require 'term+ nil t)
;  (require 'xterm-256color nil t))

;;; Dired Settings
(setq dired-listing-switches "-alh")
(setq dired-dwim-target t)

;;; Ediff Settiings
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; Customize Settings
(setq custom-file (concat user-emacs-top-directory "customize.el"))
(load custom-file)
(put 'downcase-region 'disabled nil)
