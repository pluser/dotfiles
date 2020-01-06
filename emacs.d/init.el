;;; init.el --- A emacs setting file
;;; Commentary:
;;;  Heavily personalized Emacs configuration file
;;; Code:

;;; Debug Setting {{{
;(setq debug-on-error t)
;;; }}}

;;; Local variable {{{
(when (< emacs-major-version 23)
	(defvar user-emacs-directory (file-name-directory (or buffer-file-name load-file-name))))
;;; }}}

;;; A hack for cache {{{
(defvar user-emacs-top-directory user-emacs-directory)
(setq user-emacs-directory (expand-file-name "cache/" (or user-emacs-top-directory user-emacs-directory)))
(defun init/locate-user-config (filename)
	"Return an path for Emacs configuration FILENAME."
	(let ((user-emacs-directory user-emacs-top-directory))
		(locate-user-emacs-file filename)))
;;; }}}

;;; Set load-path {{{
;;(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (init/locate-user-config "elisps/"))
(add-to-list 'load-path (init/locate-user-config "monkeys/"))
(when (< emacs-major-version 24)	; if emacs didn't have package system
	(add-to-list 'load-path (init/locate-user-config "packages/"))
	(let ((default-directory (init/locate-user-config "packages/")))
		(normal-top-level-add-subdirs-to-load-path)))
(add-to-list 'load-path (init/locate-user-config "local/"))
(let ((default-directory (init/locate-user-config "local/")))
	(normal-top-level-add-subdirs-to-load-path))
;;; }}}

;;; Set exec-path {{{
(defun init/add-exec-path (path-list)
	"Add valid PATH-LIST or PATH to 'exec-path."
	(interactive "sAdd 'exec-path: ")
	(if (listp path-list)
			(dolist (path path-list)
				(when (file-directory-p path)
					(add-to-list 'exec-path path t)))
		(when (file-directory-p path-list)
			(add-to-list 'exec-path path-list t))))
(defun init/add-exec-path-from-shell ()
	"Get and add PATH to 'exec-path from shell."
	(let ((path-from-shell (split-string (replace-regexp-in-string "[ \t\n]*" "" (shell-command-to-string "${SHELL} -c 'echo ${PATH}'")) path-separator)))
		(init/add-exec-path path-from-shell)))
(init/add-exec-path (init/locate-user-config "external-tools/npm/bin/"))
(init/add-exec-path-from-shell)
;;(add-to-list 'exec-path "~/.local/bin/")
;;(add-to-list 'exec-path "~/.go/bin/")
;;; }}}

;;; External file loader {{{
(defun ext-config (filename &optional ignore-missing)
	"Load FILENAME and evaluate it, if it exist.
If the file does not exist, issue an warning message but error.
If IGNORE-MISSING is non-nil, the warning message will be suppress even if the file does not exist."
	(let ((filepath (init/locate-user-config filename)))
		(if (file-readable-p filepath)
				(load-file filepath)
			(unless ignore-missing
				(warn "Failed to load file '%s' by '%s'."
							filename
							(file-name-nondirectory (or buffer-file-name load-file-name)))))))
;;; }}}

;;; Font Setting {{{
(when (and (display-graphic-p) (file-readable-p (init/locate-user-config "fonts.el")))
	(ext-config "fonts.el")
	;(set-fontset-font "fontset-startup" 'ascii "Inconsolata")
	;(set-fontset-font "fontset-startup" 'japanese-jisx0208 "Migu 1M")
	;(set-face-attribute 'default nil :height 180))
	;(add-to-list 'default-frame-alist '(font . "fontset-kawasemi"))
	(set-face-attribute 'default nil :family "Inconsolata" :height 120)
  (set-fontset-font nil 'cp932 (font-spec :family "Migu 1M")))
	;(set-face-attribute 'default nil 28))
	;(set-face-font 'default "fontset-kawasemi"))
	;(set-face-attribute 'default nil :fontset "fontset-kawasemi"))
;	(set-frame-font "fontset-kawasemi"))
;	(if (< emacs-major-version 24)
;		(progn (set-face-font 'default "fontset-mejiro")
;					 (set-frame-font "fontset-kawasemi"))
;		(progn (add-to-list 'default-frame-alist '(font . "fontset-mejiro"))
;					 (set-face-attribute 'variable-pitch nil :fontset "fontset-kawasemi")
;					 (set-face-attribute 'fixed-pitch nil :fontset "fontset-mejiro")
;					 ;(set-variable use-default-font-for-symbols nil)
;					 )))
;;; }}}

;;; Custom Patch Loading {{{
(ext-config "monkey.el" t)
;;; }}}

;;; Package Management {{{
(defmacro package-config (package &rest body)
	"If loaded PACKAGE, evaluate BODY."
	(declare (indent defun))
	(if (fboundp 'with-eval-after-load)
			`(with-eval-after-load ,package ,@body)	; with-eval- is more better.
		`(eval-after-load ,package (lambda () ,@body))))

(defun package-invoke (package-initiater &optional hook autoinstall)
	"Set the up PACKAGE-INITIATER in startup sequence.
If HOOK is non-nil, hang invoking package into HOOK instead of startup sequence.  If AUTOINSTALL is non-nil and the package was not installed, installed automatically."
	(if (and (fboundp package-initiater) (not (equal hook 'require-only)))
			(add-hook (or hook 'emacs-startup-hook) package-initiater)
		(unless (require package-initiater nil t)
			(if (featurep 'package)
				(let ((pkg (or autoinstall package-initiater)))
					(unless (package-installed-p pkg)
						(unless package-archive-contents (package-refresh-contents))
						(condition-case err (package-install pkg)
								(file-error (warn "Failed to install a package '%s'." err)))))
				(warn "Failed to start up a package '%s'." package-initiater)))))

(defmacro package-depend (package &rest body)
	"Declare the BODY depends on PACKAGE."
	(declare (indent defun))
	`(when (or (featurep ,package) (fboundp ,package)) ,@body))

(package-config 'package
	(let ((pkgs
				'(("melpa" . "https://melpa.org/packages/")
					("melpa-stable" . "https://stable.melpa.org/packages/")
					("org" . "https://orgmode.org/elpa/"))))
		(dolist (pkg pkgs)
			(add-to-list 'package-archives pkg t)))
	(setq package-archive-priorities
				'(("melpa" . 20)
					("marmalade" . 10)
					("gnu" . 7)
					("melpa-stable" . 3)
					("org" . 6)))
	(set-variable 'package-user-dir (init/locate-user-config "packages/")))
(package-initialize)

(when (not (package-installed-p 'use-package))
	(package-install 'use-package))

;	(dolist (pkg init/autoinstall-packages)
;		(unless (package-installed-p pkg)
;			(unless package-archive-contents (package-refresh-contents))
;			(package-install pkg))))
;(package-invoke 'package)

;;; }}}

;;; Language Setting {{{
;;;	 If you want to know charset priority, (print (charset-priority-list))
(set-language-environment 'Japanese)	; set for using japanese ONLY
(set-variable 'default-input-method "japanese-skk")
(prefer-coding-system 'utf-8)
(when (eq system-type 'darwin)
	(require 'ucs-normalize)
	(set-file-name-coding-system 'utf-8-hfs))
;;(reset-language-environment)
;;(set-charset-priority 'unicode)
(use-package uim
	:config
	(set-variable 'default-input-method 'japanese-google-cgiapi-jp-uim))
;;(package-invoke 'uim-leim)
(when (eq window-system 'mac)
	(package-invoke 'mac-auto-ascii-mode))
;;; }}}

;;; User Interface Setting {{{
(set-frame-parameter nil 'inhibit-double-buffering t) ; workaround for inordinate cursor blinking.
(transient-mark-mode 0)
(set-variable 'visible-bell t)
(set-variable 'ring-bell-function 'ignore)
(set-variable 'use-dialog-box nil)
(if (< emacs-major-version 24) (tool-bar-mode nil) (tool-bar-mode 0))
(if (< emacs-major-version 24) (menu-bar-mode nil) (menu-bar-mode 0))
;;; }}}

;;; Color / Theme Setting {{{
(set-variable 'custom-theme-directory (init/locate-user-config "theme/"))
(if (fboundp 'load-theme)
	(progn
			(package-invoke 'doom-themes)
			(load-theme 'doom-city-lights t))
;			(load-theme 'deeper-blue)
;			(load-theme 'pluser-deeper-blue t))
	(package-config 'color-theme		; Extension: color-theme
		(color-theme-initialize)
		(color-theme-deep-blue))
	(package-invoke 'color-theme))
;;; }}}

;;; File Opener {{{
(use-package filecache
	:config
	(file-cache-add-file (init/locate-user-config "init.el")))
;(package-invoke 'filecache)
(use-package recentf
	:config
	(set-variable 'recentf-max-saved-items 2000))
;(package-invoke 'recentf-mode)
;;; }}}

;;; Diff Setting {{{
;(package-config 'diff)
;;; }}}

;;; Code Ornament / Visual {{{
(show-paren-mode t)
(setq show-paren-style 'mixed)
(setq scroll-conservatively 1) ; amount of scrolling
;(set-variable 'scroll-margin 5) ; keep lines on scrolling
(setq line-number-mode t) ; show line number at modeline
(setq column-number-mode t) ; show column number at modeline
(setq indicate-empty-lines t)
(setq-default truncate-lines nil)
(setq auto-hscroll-mode 'current-line) ; scroll one line or whole screen
(when (< emacs-major-version 26) (global-linum-mode t)) ; show line number at left margin
(global-hl-line-mode t) ; highlight a line cursor is on
(defun init/prog-mode-editor-style ()
	"Settings for 'prog-mode."
	(set-variable 'display-line-numbers 'relative)
	(set-variable 'indicate-empty-lines t))
(add-hook 'prog-mode-hook 'init/prog-mode-editor-style)
(defface dspace-emphasis '((t :background "red")) "Used for dspace emphasis")
(defun init/emphasis-dspace ()
	"Emphasis ideographic space."
	(font-lock-add-keywords nil '(("　" 0 'dspace-emphasis t))))
(add-hook 'prog-mode-hook 'init/emphasis-dspace)
;;; }}}

;;; Code styling {{{
;; tabs and indentation
(setq-default indent-tabs-mode t)
(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(set-variable 'c-default-style
			'((c++-mode . "k&r")
				(java-mode . "java")
				(awk-mode . "awk")
				(other . "gnu")))
(set-variable 'c-tab-always-indent 'indent)
;;(setq-default require-final-newline nil)
;;(set-variable 'mode-require-final-newline nil)
;;; }}}

;;; Completion {{{
(setq completion-ignore-case t)
;;(setq read-file-name-completion-ignore-case t)
;;(partial-completion-mode t)
;;(icomplete-mode t)
;;; }}}

;;; History {{{
(use-package savehist
	:config
	(set-variable 'history-length 100000))
;(package-invoke 'savehist-mode)
;;; }}}

;;; Compression {{{
(auto-compression-mode t)
;;; }}}

;;; Define global keymap {{{
(define-key global-map (kbd "C-h") 'backward-delete-char-untabify)
(define-key global-map (kbd "<muhenkan>") 'pop-to-mark-command)
(define-key global-map (kbd "<menu>") 'repeat)
;;; }}}

;;; Load Private confidential file {{{
(ext-config "private.el" t)
;;; }}}

;;; Cleaning major/minor mode in modeline {{{
(defvar init/mode-line-cleaner-alist
	'((emacs-lisp-mode . "ELisp")
		(helm-mode . "")
		(company-mode . "")
		(whitespace-mode . "")
		(which-key-mode . "")
		(yas-minor-mode . "")))
(defun init/clean-modeline ()
	"Cleaning modeline up."
	(dolist (mode init/mode-line-cleaner-alist)
		(when (eq (car mode) major-mode)
			(setq mode-name (cdr mode)))
		(when (assq (car mode) minor-mode-alist)
			(let ((m (assq (car mode) minor-mode-alist)))
				(setcdr m (list (cdr mode)))))))
(add-hook 'after-change-major-mode-hook 'init/clean-modeline)
(add-hook 'emacs-startup-hook 'init/clean-modeline)
(use-package telephone-line		; Extension: telephone-line
	:disabled
	:config
	(telephone-line-mode))
;(package-invoke 'telephone-line)
(use-package doom-modeline		; Extension: doom-modeline
;	(set-variable doom-modeline-height 1)
;	(set-face-attribute 'mode-line nil :height 100)
;	(set-face-attribute 'mode-line-inactive nil :height 100)
	)
;(package-invoke 'doom-modeline)
;(package-invoke 'doom-modeline-mode nil 'doom-modeline)

;;;}}}

;;; Suppress warning {{{
;(setq ad-redefinition-action 'accept)
;;; }}}

;;; Backcup, Autosave and Locking Settings {{{
(set-variable 'auto-save-list-file-prefix (locate-user-emacs-file "auto-save-list/autosaves-")) ; the variable sat at very early on emacs startup, so it should be set again.
(add-to-list 'auto-save-file-name-transforms `(".*" ,(locate-user-emacs-file "auto-save/") t))
(add-to-list 'backup-directory-alist `(".*" . ,(locate-user-emacs-file "backup-files/")))
(set-variable 'kept-new-versions 10)
(set-variable 'kept-old-versions 0)
(set-variable 'version-control t)
(set-variable 'delete-old-versions t)
;;; }}}

;;; File Local Variable Settings {{{
(use-package files
	:config
	(add-to-list 'safe-local-variable-values '(origami-fold-style . triple-braces))
	(add-to-list 'safe-local-eval-forms '(outline-minor-mode t)))
;;; }}}


;;; Major Mode {{{

;;; Web-mode Settings {{{
(use-package web-mode		; Extension: web-mode
	:config
	(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
	(set-variable 'web-mode-code-indent-offset 4)
	(set-variable 'web-mode-markup-indent-offset 4)
	(set-variable 'web-mode-style-padding 4))
;;; }}}

;;; JS2-mode Settings {{{
(use-package js2-mode		; Extension: js2-mode
	:config
	(add-to-list 'auto-mode-alist '("\\.html?\\'" . js2-mode))
	;;(add-hook 'js-mode-hook 'init/company-tern-enable)
	(package-depend 'lsp
		(add-hook 'js2-mode-hook 'lsp)))
;;; }}}

;;; Python-mode Settings {{{
(use-package python		; Extension: python-mode
	:config
	(defun init/setting-python-mode ()
		(setq indent-tabs-mode t)
		(setq python-indent-offset 4)
		(setq tab-width 4))
	(add-hook 'python-mode-hook 'init/setting-python-mode)
	(package-depend 'lsp
		(package-invoke 'lsp 'python-mode-hook)))
;;; }}}

;;; CC-Mode Settings {{{
(use-package cc-mode
	:config
	;;(set-default 'c-hungry-delete-key t)
	(package-depend 'ensime-mode
		(add-hook 'java-mode-hook 'ensime-mode))
	(package-depend 'lsp
		(require 'ccls nil t) ; load ccls if it exists
		(package-invoke 'lsp 'c-mode-hook)))
;;; }}}

;;; Jinja2 Settings {{{
(use-package jinja2-mode
	:config
	(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . jinja2-mode)))
;;; }}}

;;; Dart Settings {{{
(use-package dart-mode
	:config
	(defun init/setting-dart-mode ()
		(setq tab-width 4))
	(package-depend 'lsp
		(set-variable 'lsp-clients-dart-server-command "~/.local/dart-package/bin/dart_language_server")
		(package-invoke 'lsp 'dart-mode-hook))
	(defun init/flutter-hot-reload ()
		(interactive)
			"Send a signal to daemon to hot reload."
			;;(start-process-shell-command "emacs-flutter-hot-reloader" "testproc" "kill -SIGUSR1 \"$(pgrep -f flutter_tool)\"")
			(start-process "emacs-flutter-hot-reloader" nil "pkill" "-SIGUSR1" "-f" "flutter_tool"))
	(defun init/flutter-hot-reload-enable()
		(interactive)
			"Enable flutter hot reload on save."
		(add-hook 'after-save-hook 'init/flutter-hot-reload t t))
	(defun init/flutter-hot-reload-disable()
		(interactive)
			"Disable flutter hot reload on save."
			(remove-hook 'after-save-hook 'init/flutter-hot-reload t))
	(add-hook 'dart-mode-hook 'init/setting-dart-mode)
	(add-hook 'dart-mode-hook 'init/flutter-hot-reload-enable))
;;; }}}
;;; Major Mode }}}

;;; Extensions {{{
;;; Hydra Settings {{{
(use-package hydra		; Extension: hydra
	)
;(package-invoke 'hydra)
;;; }}}

;;; Counsel, Swiper, Ivy Settings {{{
(use-package ivy		; Extension: ivy
	:config
	(set-variable 'ivy-use-virtual-buffers t))
(use-package swiper		; Extension: swiper
	)
(use-package counsel		; Extension: counsel
	)
;	(counsel-mode))
;(package-invoke 'counsel)
;;; }}}

;;; Avy Settings {{{
(use-package avy		; Extension: avy
	)
;(avy-setup-default))
;(package-invoke 'avy)
;;; }}}

;;; Dashbord Settings {{{
(use-package dashboard		; Extension: dashboard
	)
;	(dashboard-setup-startup-hook))
;(package-invoke 'dashboard)
;;; }}}

;;; Which-Key Settings {{{
(use-package which-key		; Extension: which-key
	:config
	(which-key-setup-side-window-right-bottom))
;(package-invoke 'which-key-mode nil 'which-key)
;;; }}}

;;; Gtags(GNU GLOBAL) Settings {{{
(use-package gtags)		; Extension: gtags
;(package-invoke 'gtags-mode 'prog-mode-hook)
;;; }}}

;;; Flycheck Settings {{{
(use-package flycheck)		; Extension: flycheck
;(package-invoke 'flycheck-mode 'prog-mode-hook 'flycheck)
;;; }}}

;;; TeX/LaTeX Settings {{{
(use-package tex		; Extension: tex
	:config
	(set-variable 'TeX-engine 'luatex)
	(add-to-list 'TeX-view-program-list '("MuPDF" "mupdf %o"))
	(setq TeX-view-program-selection '((output-pdf "MuPDF"))))

(use-package latex-math-preview
	:config
	(add-to-list 'latex-math-preview-latex-usepackage-for-not-tex-file "\\usepackage{tikz}")
	;(setq-default latex-math-preview-tex-to-png-for-preview '(lualatex-to-pdf convert))
	;;(setq-default latex-math-preview-tex-to-png-for-preview '(lualatex-to-pdf gs-to-png))
	;(setq-default latex-math-preview-command-trim-option-alist '((dvipng "-T" "tight") (dvips-to-ps "-E" "-x" "3000") (dvips-to-eps "-E" "-x" "3000") (convert "-trim"))"Options of commands to trim margin.")
	;(add-to-list 'latex-math-preview-command-create-argument-alist (convert . init/latex-math-preview-argument-convert))
	(defun init/latex-math-preview-argument-convert (command input)
		(let* ((out (latex-math-preview-change-file-extension command input))
					(args (append (latex-math-preview-get-command-option command) (list input output))))
			(cons out args)))
	;(setq-default latex-math-preview-trim-image t)
	(latex-math-preview-define-convert-function 'convert))
;;; }}}

;;; Org-mode Settings {{{
(use-package org		; Extension: org
	:config
	;;(set-variable 'org-babel-no-eval-on-ctrl-c-ctrl-c t)
	(set-variable 'org-startup-truncated nil)
	(org-babel-do-load-languages
	 'org-babel-load-languages
	 '((emacs-lisp . t) (dot . t)))
	(string-match "\\\\documentclass{article}" org-format-latex-header)
	(set-variable 'org-format-latex-header (replace-match "\\documentclass[autodetect-engine,dvipdfmx-if-dvi,ja=standard,enablejfam=true]{bxjsarticle}" t t org-format-latex-header))
	(add-to-list 'org-preview-latex-process-alist '(xelatex-dvisvgm :programs ("xelatex" "dvisvgm") :description "xdv > svg" :message "you need to install the programs: xelatex and dvisvgm." :use-xcolor t :image-input-type "xdv" :image-output-type "svg" :image-size-adjust (2.0 . 2.0) :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f") :image-converter ("dvisvgm %f -n -b min -c %S -o %O")))
	(set-variable 'org-preview-latex-default-process 'xelatex-dvisvgm)
	(use-package ox
		:config
		(set-variable 'org-export-allow-bind-keywords t))
	(use-package ox-pandoc
		:config
		(set-variable 'org-pandoc-options-for-latex-pdf '((pdf-engine . "lualatex"))))
	(use-package ox-latex
		:config
		(add-to-list
		 'org-latex-classes
		 '("ltjsarticle" "\\documentclass[autodetect-engine,dvipdfmx-if-dvi,ja=standard]{bxjsarticle}"
			 ("\\section{%s}" . "\\section*{%s}")
			 ("\\subsection{%s}" . "\\subsection*{%s}")
			 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
			 ("\\paragraph{%s}" . "\\paragraph*{%s}")
			 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
		(add-to-list
		 'org-latex-classes
		 '("jxelatex" "\\documentclass[autodetect-engine,dvipdfmx-if-dvi,ja=standard,enablejfam=true]{bxjsarticle}"
			 ("\\section{%s}" . "\\section*{%s}")
			 ("\\subsection{%s}" . "\\subsection*{%s}")
			 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
			 ("\\paragraph{%s}" . "\\paragraph*{%s}")
			 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
		(set-variable 'org-latex-default-class "ltjsarticle")
		(set-variable 'org-latex-compiler "lualatex")
		))
;;; }}}

;;; Helm Settings {{{
(use-package helm		; Extension: helm
	:config
	(package-invoke 'helm-config 'require-only)
	(define-key global-map (kbd "C-x b") 'helm-mini)
	(define-key global-map (kbd "C-x f") 'helm-multi-files)
	(define-key global-map (kbd "M-x") 'helm-M-x)
	(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
	(define-key global-map (kbd "C-x /") 'helm-occur)
	(define-key isearch-mode-map (kbd "C-t") 'helm-occur-from-isearch)
	;;(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
	;;(define-key helm-map (kbd "C-z") 'helm-select-action)
	)
;(package-invoke 'helm-mode nil 'helm)
;;; }}}

;;; Evil Settings {{{
(use-package evil		; Extension: evil
	:config
	(dolist (state '(normal motion visual))
		(define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "h") 'evil-backward-char)
		(define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "t") 'evil-next-line)
		(define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "n") 'evil-previous-line)
		(define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "s") 'evil-forward-char)
		(define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "m") 'evil-search-next)
		(define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "M") 'evil-search-previous))
	(define-key evil-insert-state-map (kbd "M-SPC") 'evil-normal-state)
	(define-key evil-insert-state-map (kbd "<henkan>") 'evil-normal-state)
	(define-key evil-insert-state-map (kbd "C-e") nil)
	(define-key evil-insert-state-map (kbd "C-t") nil)
	(set-variable 'evil-move-cursor-back nil)
	;;(set-variable 'evil-default-state 'emacs)
	(set-variable 'evil-echo-state nil)
	(set-variable 'evil-insert-state-cursor nil)
	(eval-after-load 'help-mode '(evil-make-overriding-map help-mode-map))
	(package-depend
		(global-evil-surround-mode))
	(add-to-list 'evil-emacs-state-modes 'text-mode)
	(add-to-list 'evil-emacs-state-modes 'org-mode))
;(autoload 'evil-local-mode "evil"); to support lazy load of eval-local-mode minor mode
;(package-invoke 'evil-local-mode 'prog-mode-hook 'evil)
;(package-invoke 'evil-mode 'prog-mode-hook 'evil)
;(package-invoke 'evil-surround 'evil-mode-hook)
;;; }}}

;;; Company Settings {{{
(use-package company		; Extension: company
	:config
	(set-variable 'company-transformers '(company-sort-by-backend-importance company-sort-by-occurrence))
	(use-package company-statistics		; Extension: company-statistics
		:config
		(add-to-list 'company-transformars 'company-sort-by-statistics))
	(set-variable 'company-frontends '(company-pseudo-tooltip-unless-just-one-frontend-with-delay company-echo-metadata-frontend company-preview-frontend))
	;;(define-key global-map (kbd "C-t") 'company-complete)
	;;(define-key company-active-map 'company-complete-common nil)
	(define-key company-mode-map (kbd "C-t") 'company-complete-common)
	(set-variable 'company-idle-delay 0.5)
	(set-variable 'company-tooltip-idle-delay 0.5)
	(set-variable 'company-selection-wrap-around t)
	(setq company-show-numbers t)
	(use-package company-math		; Extension: company-math
		:config
		(add-to-list 'company-backends 'company-math-symbols-unicode))
	(use-package company-tern		; Extension: company-tern
		:config
		(set-variable 'company-tern-property-marker nil))
	(defun init/company-jedi-enable ()
		;(make-local-variable 'company-backends)
		(add-to-list 'company-backends 'company-jedi))
	(defun init/company-tern-enable ()
		(interactive)
		(tern-mode t)
		;(make-local-variable 'company-backends)
		(add-to-list 'company-backends '(company-tern :with company-dabbrev-code))))
;(package-invoke 'company-mode 'prog-mode-hook 'company)
;;; }}}

;;; LSP Settings {{{
(use-package lsp		; Extension: lsp-mode
	:config
	;(package-invoke 'lsp-clients 'require-only)
	;(set-variable 'lsp-enable-indentation nil)
	(set-variable 'lsp-prefer-flymake :none)
	(package-depend 'company-lsp
		(defun init/company-lsp-enable ()
			(add-to-list 'company-backends 'company-lsp))
		(add-hook 'lsp-mode-hook 'init/company-lsp-enable)))
(use-package lsp-ui		; Extension: lsp-ui
	;(set-variable 'lsp-ui-sideline-enable nil)
	;(set-variable 'lsp-ui-doc-enable nil)
	)
;(package-invoke 'lsp 'require-only 'lsp-mode)
;;; }}}

;;; Magit Settings {{{
(use-package magit		; Extension: magit
	:config
	(define-key global-map (kbd "<f12>") 'magit-status)
	(set-variable 'magit-last-seen-setup-instruction "1.4.0"))
;(package-invoke 'magit)
;;; }}}

;;; Yasnippet Settings {{{
(use-package yasnippet		; Extension: yasnippet
	:config
	(set-variable 'yas-snippet-dirs (list (init/locate-user-config "yasnippets/"))))
;(package-invoke 'yas-minor-mode 'prog-mode-hook 'yasnippet)
;(package-invoke 'yas-minor-mode 'org-mode-hook 'yasnippet)
;;; }}}

;;; Projectile Settings {{{
(use-package projectile		; Extension: projectile
	:config
	;;(set-variable 'projectile-mode-line (format " Proj[%s]" (projectile-project-name)))
	(set-variable 'projectile-mode-line-prefix " Pj"))
;(package-invoke 'projectile-mode nil 'projectile)
;;; }}}

;;; Auto-Complete Settings {{{
(use-package auto-complete		; Extension: auto-complete
	:config
	;;(add-to-list 'ac-dictionary-directories (locate-user-emacs-file "acdict/"))
	(require 'auto-complete-config)
	(ac-config-default)
	(set-variable 'ac-auto-show-menu nil)
	(set-variable 'ac-use-quick-help nil)
	;;(define-key ac-mode-map "\M-/" 'auto-complete)
	)
;;; }}}

;;; Elpy Settings {{{
(use-package elpy		; Extension: elpy
	:config
	(set-variable 'elpy-modules (delete 'elpy-module-highlight-indentation elpy-modules)))
;;; }}}

;;; Undo-Tree Settings {{{
(use-package undo-tree		; Extension: undo-tree
	:config
	(set-variable 'undo-tree-mode-lighter ""))
;;; }}}

;;; Mozc Settings {{{
(use-package mozc		; Extension: mozc
	:config
	(set-variable 'mozc-candidate-style 'echo-area))
;(package-invoke 'mozc)
;;; }}}

;;; Skk Settings {{{
(use-package skk		; Extension: SKK
	:init
	(setq skk-user-directory (locate-user-emacs-file "ddskk/"))
	:config
	;;(define-key global-map "\C-x\C-j" 'skk-mode)
	(set-variable 'skk-henkan-show-candidates-keys (list ?a ?o ?e ?u ?h ?t ?n ?s))
	(set-variable 'skk-indicator-use-cursor-color nil)
	(set-variable 'skk-kakutei-key (kbd "C-t"))
	(set-variable 'skk-use-auto-kutouten t)
	(set-variable 'skk-background-mode 'dark) ; For DDSKK 15.1 Hot-Fix
	(require 'skk-hint nil t)
	(when (require 'skk-search-web nil t)
		(add-to-list 'skk-search-prog-list '(skk-search-web 'skk-google-cgi-api-for-japanese-input) t)
		(add-to-list 'skk-search-prog-list '(skk-search-web 'skk-google-suggest) t))
	(defun my/skk-kutouten-style-ja ()
		(interactive)
		(set-variable 'skk-kutouten-type 'jp))
	(defun my/skk-kutouten-style-en ()
		(interactive)
		(set-variable 'skk-kutouten-type 'en))
	(defun my/replace-punctual-style ()
  (interactive)
  (let ((prefer-period (if (consp skk-kutouten-type) (car skk-kutouten-type) (cadr (assoc skk-kutouten-type skk-kuten-touten-alist))))
		(prefer-comma (if (consp skk-kutouten-type) (cdr skk-kutouten-type) (cddr (assoc skk-kutouten-type skk-kuten-touten-alist)))))
	(save-excursion
	  (goto-char 0)
	  (while (search-forward "、" nil t) (replace-match prefer-comma nil t))
	  (goto-char 0)
	  (while (search-forward "，" nil t) (replace-match prefer-comma nil t))
	  (goto-char 0)
	  (while (search-forward "。" nil t) (replace-match prefer-period nil t))
	  (goto-char 0)
	  (while (search-forward "．" nil t) (replace-match prefer-period nil t))))))
;(package-invoke 'skk-preload 'text-mode-hook 'ddskk)
;;; }}}

;;; w3m Settings {{{
;(use-package w3m-filter		; Extension: w3m
	;;(defun w3m-filter-alc-lay (url) (w3m-filter-delete-regions url "<!-- interest_match_relevant_zone_start -->" "<!-- ▲ 英辞郎ヘッダ ▲ -->"))
;	(defun w3m-filter-alc-lay (url) (w3m-filter-delete-regions url "<!-- interest_match_relevant_zone_start -->" "<!-- ▼ 検索結果本体 ▼ -->"))
;	(add-to-list 'w3m-filter-configuration '(t "Suck!" "\\`http://eow\\.alc\\.co\\.jp/search" w3m-filter-alc-lay)))
;;; }}}

;;; Whitespace Settings {{{
(use-package whitespace		; Extension: whitespace
	:config
	(set-variable 'whitespace-style '(face tabs trailing space-before-tab tab-mark)))
;(package-invoke 'whitespace-mode 'prog-mode-hook)
;;; }}}

;;; Xah-math-input-mode Settings {{{
(use-package xah-math-input
	:config
	(puthash "wb" "◦" xah-math-input-abrvs))
;;; }}}

;;; Uniquify Settings {{{
(use-package uniquify
	:config
	(set-variable 'uniquify-buffer-name-style 'post-forward-angle-brackets))
;(package-invoke 'uniquify)
;;; }}}

;;; Extension }}}

;;; Tramp Settings {{{
;; A workaround for Tramp for text corruption.
;;(set-variable 'tramp-remote-process-environment (quote ("HISTFILE=$HOME/.tramp_history" "HISTSIZE=1" "LC_MESSAGE=C" "TERM=dumb" "EMACS=t" "INSIDE_EMACS='24.3.1,tramp:2.2.6-24.3'" "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH=" "PAGER=\"\"" "autocorrect=" "correct=")))
;;; }}}

;;; Terminal Emulator Settings {{{
;;(when (require 'term+ nil t)
;;(require 'xterm-256color nil t))
;;; }}}

;;; Dired Settings {{{
(use-package dired
	:config
	(set-variable 'dired-listing-switches "-alh")
	(set-variable 'dired-dwim-target t))
;;; }}}

;;; Ediff Settiings {{{
(use-package ediff
	:config
	(set-variable 'ediff-window-setup-function 'ediff-setup-windows-plain))
;;; }}}

;;; Spacemacs {{{
(setenv "SPACEMACSDIR" "/tmp/spacemacsconf")
(setq spacemacs-start-directory (init/locate-user-config "spacemacs/"))
(setq-default dotspacemacs-themes '(deeper-blue))
(setq-default dotspacemacs-default-font '("Inconsolata"
                                          :size 24
                                          :weight normal
                                          :width normal
                                          :powerline-scale 1.1))
;(ext-config "spacemacs/init.el")
;;; }}}

;;; Customize Settings {{{
(setq custom-file (init/locate-user-config "customize.el"))
(load custom-file)
;;; }}}

(provide 'init)
;;; init.el ends here

;;; Local Variables:
;;; mode: Emacs-Lisp
;;; coding: utf-8
;;; lisp-body-indent: 2
;;; tab-width: 2
;;; origami-fold-style: triple-braces
;;; eval: (outline-minor-mode t)
;;; outline-regexp: ";;;.* {{{"
;;; End:
