;;; -*- lexical-binding: t; -*-
;;; init.el --- A emacs setting file
;;; Commentary:
;;;  Heavily personalized Emacs configuration file
;;; Code:

;;; Debug Setting {{{
(setq debug-on-error t)
(setq warning-minimum-level :error)
;;; }}}

;; Minimize garbage collection during startup {{{
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23)))) ; 8MiB
;;; }}}

;;; Local variable {{{
(when (< emacs-major-version 23)
	(defvar user-emacs-directory (file-name-directory (or buffer-file-name load-file-name))))
;;; }}}

;;; A hack for cache {{{
(defvar user-emacs-top-directory user-emacs-directory)
(setq user-emacs-directory (expand-file-name "var/" (or user-emacs-top-directory user-emacs-directory)))
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
(setenv "PATH" (shell-command-to-string "${SHELL} -c 'echo ${PATH}'"))
(init/add-exec-path (init/locate-user-config "external-tools/npm/bin/"))
(init/add-exec-path-from-shell)
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
	;(ext-config "fonts.el")
	(set-face-attribute 'default nil :family "Inconsolata" :height 180)
  (set-fontset-font nil 'cp932 (font-spec :family "Migu 1M")))
;;; }}}

;;; Custom Patch Loading {{{
(ext-config "monkey.el" t)
;;; }}}

;;; Package Management {{{
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(use-package straight
	:custom
	(use-package-always-defer t)
	(use-package-verbose t)
	(use-package-compute-statistics t)
	(use-package-always-ensure t)
	(straight-use-package-by-default t))

;;; }}}

;;; Color / Theme Setting {{{
(use-package doom-themes
	:demand
	:config
	(load-theme 'doom-city-lights t))
;; }}}

;;; Language Setting {{{
;;;	 If you want to know charset priority, (print (charset-priority-list))
(set-language-environment 'Japanese)	; set for using japanese ONLY
(set-variable 'default-input-method "japanese-skk")
(prefer-coding-system 'utf-8)
(when (eq system-type 'darwin)
	(require 'ucs-normalize)
	(set-file-name-coding-system 'utf-8-hfs))
(use-package uim
	:disabled
	:config
	(set-variable 'default-input-method 'japanese-google-cgiapi-jp-uim))
(when (eq window-system 'mac)
  (package-invoke 'mac-auto-ascii-mode))
(defun my/replace-kutouten-from-ja-to-en ()
  (interactive)
  (format-replace-strings '(("、" . "，") ("。" . "．"))))
(defun my/replace-kutouten-from-en-to-ja ()
  (interactive)
  (format-replace-strings '(("，" . "、") ("．" . "。"))))
;;; }}}

;;; User Interface Setting {{{
;(set-frame-parameter nil 'inhibit-double-buffering t) ; workaround for inordinate cursor blinking.
(transient-mark-mode 0)
(set-variable 'visible-bell t)
(set-variable 'ring-bell-function 'ignore)
(set-variable 'use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)
(tool-bar-mode 0)
(menu-bar-mode 0)
;;; }}}

;;; File Opener {{{
(use-package filecache
	:config
	(file-cache-add-file (init/locate-user-config "init.el")))
(use-package recentf
	:config
	(set-variable 'recentf-max-saved-items 2000))
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
;(setq-default tab-width 4)
(setq c-default-style
			'((c++-mode . "k&r")
				(java-mode . "java")
				(awk-mode . "awk")
				(other . "gnu")))
(setq c-tab-always-indent 'indent)
;;; }}}

;;; Completion {{{
(setq completion-ignore-case t)
;;; }}}

;;; History {{{
(use-package savehist
	:config
	(set-variable 'history-length 100000))
;;; }}}

;;; Compression {{{
(auto-compression-mode t)
;;; }}}

;;; Define global keymap {{{
;;(define-key global-map (kbd "C-h") 'backward-delete-char-untabify)
(define-key global-map (kbd "C-h") 'left-char)
(define-key global-map (kbd "C-t") 'next-line)
(define-key global-map (kbd "C-n") 'previous-line)
(define-key global-map (kbd "C-s") 'right-char)
(define-key global-map (kbd "<muhenkan>") 'pop-to-mark-command)
(define-key global-map (kbd "<menu>") 'repeat)
;;; }}}

;;; Load Private confidential file {{{
(ext-config "private.el" t)
;;; }}}

;;; Cleaning major/minor mode in modeline {{{
(use-package doom-modeline		; Extension: doom-modeline
	:demand
	:config
	(doom-modeline-mode 1))
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
(add-to-list 'safe-local-variable-values '(origami-fold-style . triple-braces))
(add-to-list 'safe-local-eval-forms '(outline-minor-mode t))
;;; }}}


;;; Major Mode / Application {{{

;;; Web-mode Settings {{{
(use-package web-mode		; Extension: web-mode
	:config
	(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
	(set-variable 'web-mode-code-indent-offset 4)
	(set-variable 'web-mode-markup-indent-offset 4)
	(set-variable 'web-mode-style-padding 4))
;;; }}}

;;; Rust Settings {{{
(use-package rustic		; Extension: rustic
	)
;;; }}}

;;; JS2-mode Settings {{{
(use-package js2-mode		; Extension: js2-mode
	:disabled
	:config
	(add-to-list 'auto-mode-alist '("\\.html?\\'" . js2-mode)))
;;; }}}

;;; Python-mode Settings {{{
(use-package python		; Extension: python-mode
	:config
	(defun init/setting-python-mode ()
		(setq indent-tabs-mode t)
		(setq python-indent-offset 4)
		(setq tab-width 4))
	(add-hook 'python-mode-hook 'init/setting-python-mode))
;;; }}}

;;; CC-Mode Settings {{{
(use-package cc-mode
	:config
	(set-default 'c-hungry-delete-key t))
;;; }}}

;;; Julia Settings {{{
(use-package lsp-julia
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.5"))
;;; }}}

;;; Dart Settings {{{
(use-package dart-mode
	:no-require t
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

;;; EIN: Emacs IPython Notebook {{{
(use-package ein
	:config
	(setq ein:output-area-inlined-images t))
;;; }}}

;;; Major Mode }}}

;;; Extensions {{{

;;; EditorConfig Settings {{{
(use-package editorconfig
	:init
	(editorconfig-mode 1))
;;; }}}

;;; Exec Path From Shell {{{
(use-package exec-path-from-shell		; Extension: exec-path-from-shell
	:config
	(set-variable 'exec-path-from-shell-variables '("PATH" "PYTHONUSERBASE"))
	;(set-variable 'exec-path-from-shell-arguments nil)
	(exec-path-from-shell-initialize))
;;; }}}

;;; Tree-sitter Settings {{{
(use-package tree-sitter		; Extension: tree-sitter
	:config (global-tree-sitter-mode t)
	:hook (tree-sitter-after-on . tree-sitter-hl-mode))
(use-package tree-sitter-langs		; Extension: tree-sitter-langs
	)
(use-package evil-textobj-tree-sitter
  :config
	(define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer")))
;;; }}}

;;; objed Settings {{{
(use-package objed
	:hook (prog-mode . objed-local-mode))
;;; }}}

;;; Counsel, Swiper, Ivy Settings {{{
(use-package ivy		; Extension: ivy
	:disabled
	:demand
	:custom
	(ivy-use-virtual-buffers t)
	:config
	(ivy-mode 1))
(use-package swiper		; Extension: swiper
	:disabled
	:bind
	(("C-r" . swiper-isearch)
	 ("C-l" . swiper-isearch-backward))
	(:map swiper-isearch-map
				("C-r" . swiper-C-s)
				("C-l" . swiper-isearch-C-r)))
(use-package counsel		; Extension: counsel
	:after ivy
	:demand
	:config
	(counsel-mode 1))
(use-package ivy-rich		; Extension: ivy-rich
	:after ivy
	:demand
	:config
	(ivy-rich-mode 1))
(use-package all-the-icons-ivy-rich		; Extension: all-the-icons-ivy-rich
	:after ivy
	:demand
	:config
	(all-the-icons-ivy-rich-mode 1))
(use-package ivy-prescient		; Extension: ivy-prescient
	:after ivy
	:demand
	:config
	(ivy-prescient-mode 1))
(use-package orderless		; Extension: orderless
	:disabled
	:after ivy
	:demand
	:custom
	(completion-styles '(orderless))
	(ivy-re-builders-alist '((t . orderless-ivy-re-builder))))
;;; }}}

;;; Minibuffer related Plugins {{{
(use-package vertico
  :hook ((emacs-startup . vertico-mode))
  :custom
  (vertico-count 20))
(use-package consult
	:bind
	(:map global-map
		("C-x b" . consult-buffer)
		("C-x C-b" . consult-buffer)
		("C-r" . consult-line)
		("C-l" . consult-isearch))
	(:map isearch-mode-map
		("C-r" . consult-line)
		("C-l" . consult-isearch))
	:custom
	(xref-show-xrefs-function #'consult-xref)
	(xref-show-definitions-function #'consult-xref))
(use-package marginalia
	:hook ((emacs-startup . marginalia-mode)))
(use-package orderless
	:custom
	(completion-styles '(orderless)))
(use-package embark
	:bind
	(:map global-map
				("C-'" . embark-act)))
(use-package embark-consult :after (embark consult))
;;; }}}

;;; Completion related Plugins {{{
(use-package company		; Extension: company
	:hook ((prog-mode . company-mode))
	:bind
	(:map company-mode-map
				("C-." . company-complete-common)))
(use-package company-prescient
  :after company
  :init
  (company-prescient-mode 1))
(use-package corfu
	:init
	(corfu-global-mode))
;;; }}}

;;; Xref related Plugins {{{
(use-package dumb-jump)
(use-package smart-jump
	:bind (("M-." . smart-jump-go)
				 ("M-," . smart-jump-back)
				 ("M-?" . smart-jump-references)))
;;; }}}

;;; LSP related Settings {{{
(use-package lsp-mode		; Extension: lsp-mode
	:hook ((prog-mode . lsp)
				 (lsp-mode . lsp-enable-which-key-integration))
	:commands lsp
	:custom
	(read-process-output-max (* 1024 1024)))
(use-package lsp-ui :disabled :after (lsp) :commands lsp-ui-mode)
(use-package lsp-ivy :disabled :commands lsp-ivy-workspace-symbol)
(use-package consult-lsp :after (consult lsp))
(use-package lsp-treemacs :after (lsp treemacs) :commands lsp-treemacs-errors-list)
;;; }}}

;;; Avy Settings {{{
(use-package avy		; Extension: avy
	)
;;; }}}

;;; Dashbord Settings {{{
(use-package dashboard		; Extension: dashboard
	:init
	(dashboard-setup-startup-hook)
	:custom
	(dashboard-items '((recents . 5) (bookmarks . 5))))
;;; }}}

;;; Which-Key Settings {{{
(use-package which-key		; Extension: which-key
	:hook ((emacs-startup . which-key-mode))
	:config
	(which-key-setup-side-window-right-bottom))
;;; }}}

;;; Flycheck Settings {{{
(use-package flycheck		; Extension: flycheck
	)
;;; }}}

;;; Hydra Settings {{{
(use-package hydra		; Extension: hydra
	:disabled
	)
;;; }}}

;;; TeX/LaTeX Settings {{{
(add-hook 'LaTeX-mode-hook 'outline-minor-mode)
(use-package tex		; Extension: auctex
	:straight auctex
	:ensure auctex
	:config
	(add-to-list 'TeX-view-program-selection '(output-pdf "Zathura"))
	(setq TeX-source-correlate-mode t)
	(setq TeX-parse-self t)
	(setq japanese-TeX-engine-default 'uptex)
	(setq-default TeX-engine 'luatex)
	(add-to-list 'TeX-command-list '("Latexmk" "latexmk %t" TeX-run-TeX nil (latex-mode) :help "Run Latexmk"))
  (add-to-list 'TeX-command-list '("Latexmk-upLaTeX" "latexmk -e '$latex=q/uplatex %%O %(file-line-error) %(extraopts) %S %(mode) %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/' -e '$makeindex=q/upmendex %%O -o %%D %%S/' -e '$dvipdf=q/dvipdfmx %%O -o %%D %%S/' -norc -gg -pdfdvi %t" TeX-run-TeX nil (latex-mode) :help "Run Latexmk-upLaTeX"))
  (add-to-list 'TeX-command-list '("Latexmk-LuaLaTeX" "latexmk -e '$lualatex=q/lualatex %%O %(file-line-error) %(extraopts) %S %(mode) %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/' -e '$makeindex=q/upmendex %%O -o %%D %%S/' -norc -gg -pdflua %t" TeX-run-TeX nil (latex-mode) :help "Run Latexmk-LuaLaTeX")))
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
	(set-variable 'org-export-allow-bind-keywords t)
	(use-package ox-latex
		:disabled
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
(use-package org-roam)
;;; }}}

;;; Helm Settings {{{
(use-package helm		; Extension: helm
	:disabled
	:config
	(package-invoke 'helm-config 'require-only)
	(define-key global-map (kbd "C-x b") 'helm-mini)
	(define-key global-map (kbd "C-x f") 'helm-multi-files)
	(define-key global-map (kbd "M-x") 'helm-M-x)
	(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
	(define-key global-map (kbd "C-x /") 'helm-occur)
	(define-key isearch-mode-map (kbd "C-t") 'helm-occur-from-isearch)
	)
;;; }}}

;;; Evil Settings {{{
(use-package evil		; Extension: evil
	:demand t
	:bind
	(:map evil-emacs-state-map
				("<delete>" . evil-execute-in-normal-state)
				("<f8>" . evil-toggle-fold))
	(:map evil-normal-state-map
				("<delete>" . evil-execute-in-emacs-state)
				("<f8>" . evil-toggle-fold))	
	:config
	(evil-mode 1)
	(dolist (state '(normal motion visual))
		(define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "h") 'evil-backward-char)
		(define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "t") 'evil-next-line)
		(define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "n") 'evil-previous-line)
		(define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "s") 'evil-forward-char)
		(define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "m") 'evil-search-next)
		(define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map"))) (kbd "M") 'evil-search-previous))
	(set-variable 'evil-move-cursor-back nil)
	(set-variable 'evil-default-state 'emacs)
	(set-variable 'evil-echo-state nil)
	(set-variable 'evil-insert-state-cursor nil))
(use-package evil-surround		; Extension: evil-surround
	:after evil
	:config
	(global-evil-surround-mode 1))
;;; }}}

;;; Magit Settings {{{
(use-package magit		; Extension: magit
	:bind (("<f12>" . magit-status))
	:custom
	(magit-last-seen-setup-instruction "1.4.0"))
;;; }}}

;;; Treemacs Settings {{{
(use-package treemacs		; Extension: treemacs
	:config
	(treemacs-load-all-the-icons-with-workaround-font "Inconsolata"))
(use-package treemacs-all-the-icons		; Extension: treemacs-all-the-icons
	:after treemacs
	:defer nil
	)
;;; }}}

;;; Yasnippet Settings {{{
(use-package yasnippet		; Extension: yasnippet
	:hook ((text-mode . yas-minor-mode-on) (prog-mode . yas-minor-mode-on))
	:config
	(set-variable 'yas-snippet-dirs (list (init/locate-user-config "yasnippets/")))
	(yas-reload-all))
;;; }}}

;;; Projectile Settings {{{
(use-package projectile		; Extension: projectile
	:init
	(autoload 'projectile-project-root "projectile")
	:config
	(set-variable 'projectile-mode-line-prefix " Pj"))
;;; }}}

;;; DAP Mode Settings {{{
(use-package dap-mode
	:hook (prog-mode . dap-mode))
;;; }}}

;;; Helpful Settings {{{
(use-package helpful
	:bind
	([remap describe-function] . #'helpful-callable)
	([remap describe-variable] . #'helpful-variable)
	([remap describe-command] . #'helpful-command)
	([remap describe-key] . #'helpful-key))
;;; }}}

;;; Elpy Settings {{{
(use-package elpy		; Extension: elpy
	:config
	(set-variable 'elpy-modules (delete 'elpy-module-highlight-indentation elpy-modules)))
;;; }}}

;;; Undo-Tree Settings {{{
(use-package undo-tree		; Extension: undo-tree
	:disabled
	:config
	(set-variable 'undo-tree-mode-lighter ""))
;;; }}}

;;; Mozc Settings {{{
(use-package mozc		; Extension: mozc
	:disabled
	:config
	(set-variable 'mozc-candidate-style 'echo-area))
;;; }}}

;;; Skk Settings {{{
(use-package skk		; Extension: SKK
	:ensure ddskk
	:straight ddskk
	:init
	(setq skk-user-directory (locate-user-emacs-file "ddskk/"))
	:config
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
;;; }}}

;;; Whitespace Settings {{{
(use-package whitespace		; Extension: whitespace
	:ensure nil
	:straight (:type built-in)
	:config
	(set-variable 'whitespace-style '(face tabs trailing space-before-tab tab-mark)))
;;; }}}

;;; Uniquify Settings {{{
(use-package uniquify
	:ensure nil
	:straight (:type built-in)
	:config
	(set-variable 'uniquify-buffer-name-style 'post-forward-angle-brackets))
;;; }}}

;;; Extension }}}

;;; Dired Settings {{{
(use-package dired
	:ensure nil
	:straight (:type built-in)
	:config
	(set-variable 'dired-listing-switches "-alh")
	(set-variable 'dired-dwim-target t))
;;; }}}

;;; Ediff Settiings {{{
(use-package ediff
	:ensure nil
	:straight (:type built-in)
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
(load custom-file t)
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
