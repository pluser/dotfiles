;;; init.el --- A emacs setting file
;;; Commentary:
;;;  Heavily personalized Emacs configuration file
;;; Code:

(defvar init/autoinstall-packages
	'(avy
		company
		ddskk
		evil
		flycheck
		general
		helm
		origami
		projectile))

;;; Local variable {{{
(when (< emacs-major-version 23)
	(defvar user-emacs-directory (file-name-directory (or buffer-file-name load-file-name))))
;;; }}}

;;; Set load-path {{{
;;(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "elisps/"))
(add-to-list 'load-path (concat user-emacs-directory "monkeys/"))
(when (< emacs-major-version 24)	; if emacs didn't have package system
	(add-to-list 'load-path (concat user-emacs-directory "packages/"))
	(let ((default-directory (concat user-emacs-directory "packages/")))
		(normal-top-level-add-subdirs-to-load-path)))
(add-to-list 'load-path (concat user-emacs-directory "local/"))
(let ((default-directory (concat user-emacs-directory "local/")))
	(normal-top-level-add-subdirs-to-load-path))
;;; }}}

;;; Set exec-path {{{
(add-to-list 'exec-path (concat user-emacs-directory "external-tools/npm/bin/"))
(add-to-list 'exec-path "~/.local/bin/")
(add-to-list 'exec-path "~/.go/bin/")
;;; }}}

;;; External file loader {{{
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
;;; }}}

;;; Package Management {{{
(defmacro package-config (package &rest body)
	"Load and set package settings."
	(declare (indent defun))
	(if (fboundp 'with-eval-after-load)
			`(with-eval-after-load ,package ,@body)	; with-eval- is more better.
		`(eval-after-load ,package (lambda () ,@body))))

(defun package-invoke (package-initiater &optional hook autoinstall)
	"Set the package up in startup sequence.
If HOOK is non-nil, hang invoking package into HOOK instead of startup sequence."
	(if (fboundp package-initiater)
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
	`(when (featurep ,package) ,@body))

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
	(setq package-user-dir (concat user-emacs-directory "packages/"))
	(package-initialize))
;	(dolist (pkg init/autoinstall-packages)
;		(unless (package-installed-p pkg)
;			(unless package-archive-contents (package-refresh-contents))
;			(package-install pkg))))
(package-invoke 'package)
;;; }}}

;;; Font Setting {{{
(when (and (display-graphic-p) (file-readable-p (concat user-emacs-directory "fonts.el")))
	(ext-config "fonts.el")
	(when (< emacs-major-version 24)
		;(set-face-font 'default "fontset-mejiro")
		;(set-frame-font "fontset-kawasemi")
		)

	(add-to-list 'default-frame-alist '(font . "fontset-mejiro"))
	(set-face-attribute 'variable-pitch nil :fontset "fontset-kawasemi")
	(set-face-attribute 'fixed-pitch nil :fontset "fontset-mejiro")
	)
;;; }}}

;;; Custom Patch Loading {{{
(ext-config "monkey.el" t)
;;; }}}

;;; A hack for scattering cache or auto generated files. {{{
(setq user-emacs-top-directory user-emacs-directory)
(setq user-emacs-directory (concat user-emacs-directory "cache/"))
;;; }}}

;;; Language Setting {{{
;;;	 If you want to know charset priority, (print (charset-priority-list))
;; (set-language-environment 'Japanese)	; set for using japanese ONLY
(prefer-coding-system 'utf-8)
(when (eq system-type 'darwin)
	(require 'ucs-normalize)
	(set-file-name-coding-system 'utf-8-hfs))
;;(reset-language-environment)
;;(set-charset-priority 'unicode)
(package-config 'uim
	(setq default-input-method 'japanese-google-cgiapi-jp-uim))
;;(package-invoke 'uim-leim)
(when (eq window-system 'mac)
	(package-invoke 'mac-auto-ascii-mode))
;;; }}}

;;; User Interface Setting {{{
(transient-mark-mode 0)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq use-dialog-box nil)
(if (< emacs-major-version 24) (tool-bar-mode nil) (tool-bar-mode 0))
(if (< emacs-major-version 24) (menu-bar-mode nil) (menu-bar-mode 0))
;;; }}}

;;; Color / Theme Setting {{{
(if (fboundp 'load-theme)
		(load-theme 'deeper-blue)
	(package-config 'color-theme		; Extension: color-theme
		(color-theme-initialize)
		(color-theme-deep-blue))
	(package-invoke 'color-theme))
;;; }}}

;;; File Opener {{{
(package-config 'filecache
	(file-cache-add-file (concat user-emacs-top-directory "init.el")))
(package-invoke 'filecache)
(package-config 'recentf
	(setq recentf-max-saved-items 2000))
(package-invoke 'recentf-mode)
;;; }}}

;;; Diff Setting {{{
(setq diff-switches "-u")
;;; }}}

;;; Code ornament / Visual {{{
(show-paren-mode t)
(setq show-paren-style 'mixed)
(setq scroll-conservatively 1) ; amount of scrolling
;(setq scroll-margin 5) ; keep lines on scrolling
(line-number-mode t) ; show line number at modeline
(column-number-mode t) ; show column number at modeline
(setq-default truncate-lines nil)
;(global-linum-mode t) ; show line number at left margin
(global-hl-line-mode t) ; highlight a line cursor is on
;;; }}}

;;; Code styling {{{
(setq-default indent-tabs-mode t)
(setq-default c-basic-offset 4)
(setq require-final-newline nil)
(setq mode-require-final-newline nil)
;;; }}}

;;; Completion {{{
(setq completion-ignore-case t)
;;(setq read-file-name-completion-ignore-case t)
;;(partial-completion-mode t)
(icomplete-mode t)
;;; }}}

;;; History {{{
(package-config 'savehist
	(setq history-length 10000))
(package-invoke 'savehist-mode)
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
(ext-config "private/email.el" t)
;;; }}}

;;; Cleaning major/minor mode in modeline {{{
(defvar init/mode-line-cleaner-alist
	'((emacs-lisp-mode . "ELisp")
		(whitespace-mode . "")
		(which-key-mode . "")))
(defun init/clean-modeline ()
	(dolist (mode init/mode-line-cleaner-alist)
		(when (eq (car mode) major-mode)
			(setq mode-name (cdr mode)))
		(when (assq (car mode) minor-mode-alist)
			(let ((m (assq (car mode) minor-mode-alist)))
				(setcdr m (list (cdr mode)))))))
(add-hook 'after-change-major-mode-hook 'init/clean-modeline)
;;;}}}

;;; Suppress warning {{{
;(setq ad-redefinition-action 'accept)
;;; }}}

;;; CC-Mode Settings {{{
(package-config 'cc-mode
	(package-depend 'ensime-mode
		(add-hook 'java-mode-hook 'ensime-mode)))
;;; }}}

;;; Extensions {{{

;;; Avy Settings {{{
(package-config 'avy		; Extension: avy
	(avy-setup-default))
(package-invoke 'avy)

;;; Dashbord Settings {{{
(package-config 'dashboard		; Extension: dashboard
	(dashboard-setup-startup-hook))

;;; Which-Key Settings {{{
(package-config 'which-key		; Extension: which-key
	(which-key-setup-side-window-right-bottom))
(package-invoke 'which-key-mode nil 'which-key)
;;; }}}

;;; Gtags(GNU GLOBAL) Settings {{{
(package-config 'gtags)		; Extension: gtags
(package-invoke 'gtags-mode 'prog-mode-hook)
;;; }}}

;;; Flycheck Settings {{{
(package-config 'flycheck)		; Extension: flycheck
(package-invoke 'flycheck-mode 'prog-mode-hook 'flycheck)
;;; }}}

;;; TeX/LaTeX Settings {{{
(package-config 'tex		; Extension: tex
	(setq-default TeX-engine 'luatex)
	(add-to-list 'TeX-view-program-list '("MuPDF" "mupdf %o"))
	(setq TeX-view-program-selection '((output-pdf "MuPDF"))))

(package-config 'latex-math-preview
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
	(latex-math-preview-define-convert-function convert))
;;; }}}

;;; Org-mode Settings {{{
(package-config 'org		; Extension: org
	(setq org-babel-no-eval-on-ctrl-c-ctrl-c t)
	(setq org-startup-truncated nil)
	(org-babel-do-load-languages
	 'org-babel-load-languages
	 '((emacs-lisp . t) (dot . t)))
	(package-config 'ox-pandoc
		(setq org-pandoc-options-for-latex-pdf '((latex-engine . "lualatex"))))
	(package-config 'ox-latex
		(add-to-list
		 'org-latex-classes
		 '("ltjsarticle" "\\documentclass[]{ltjsarticle}"
			 ("\\section{%s}" . "\\section*{%s}")
			 ("\\subsection{%s}" . "\\subsection*{%s}")
			 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
			 ("\\paragraph{%s}" . "\\paragraph*{%s}")
			 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
		(setq org-latex-default-class "ltjsarticle"))
		(setq org-latex-compiler "lualatex"))
;;; }}}

;;; Helm Settings {{{
(package-config 'helm-mode		; Extension: helm
	(require 'helm-config)
	(define-key global-map (kbd "C-x b") 'helm-buffers-list)
	(define-key global-map (kbd "C-x f") 'helm-find-files)
	(define-key global-map (kbd "M-x") 'helm-M-x)
	(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
	(define-key helm-map (kbd "C-z") 'helm-select-action)
	(setq helm-autoresize-max-height 80)
	(helm-autoresize-mode t))
(package-invoke 'helm-mode nil 'helm)
;;; }}}

;;; Evil Settings {{{
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
	(eval-after-load 'help-mode '(evil-make-overriding-map help-mode-map))
	(when (require 'evil-surround nil t)
		(global-evil-surround-mode)))
;(autoload 'evil-local-mode "evil"); to support lazy load of eval-local-mode minor mode
;(package-invoke 'evil-local-mode 'prog-mode-hook 'evil)
(package-invoke 'evil-mode 'prog-mode-hook 'evil)
;;; }}}

;;; Company Settings {{{
(package-config 'company		; Extension: company
	(setq company-transformers '(company-sort-by-backend-importance company-sort-by-occurrence))
	(package-config 'company-statistics		; Extension: company-statistics
		(add-to-list 'company-transformars 'company-sort-by-statistics))
	(setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend-with-delay company-echo-metadata-frontend company-preview-frontend))
	;;(define-key global-map (kbd "C-t") 'company-complete)
	;;(define-key company-active-map 'company-complete-common nil)
	(define-key company-mode-map (kbd "C-t") 'company-complete-common)
	(setq company-idle-delay 0.5)
	(setq company-tooltip-idle-delay 0.5)
	(setq company-selection-wrap-around t)
	(setq company-show-numbers t)
	(package-config 'company-math		; Extension: company-math
		(add-to-list 'company-backends 'company-math-symbols-unicode))
	(package-config 'company-tern		; Extension: company-tern
		(setq company-tern-property-marker nil))
	(defun init/company-jedi-enable ()
		(make-local-variable 'company-backends)
		(add-to-list 'company-backends 'company-jedi))
	(defun init/company-tern-enable ()
		(interactive)
		(tern-mode t)
		(make-local-variable 'company-backends)
		(add-to-list 'company-backends '(company-tern :with company-dabbrev-code))))
(package-invoke 'company-mode 'prog-mode-hook 'company)
;;; }}}

;;; LSP Settings {{{
(package-config 'lsp-mode		; Extension:  lsp-mode
	(lsp-define-stdio-client
	 lsp-python-mode
	 "python"
	 (lambda () default-directory)
	 '("pyls"))
	(defun init/lsp-python-enable ()
		(lsp-python-mode-enable))
	(lsp-define-stdio-client
	 lsp-javascript-mode
	 "javascript"
	 (lambda () default-directory)
	 '("javascript-typescript-stdio"))
	(defun init/lsp-javascript-enable ()
		(lsp-javascript-mode-enable)))
(package-invoke 'lsp-mode)
;;; }}}

;;; Magit Settings {{{
(package-config 'magit		; Extension: magit
	(define-key global-map (kbd "<f12>") 'magit-status)
	(setq magit-last-seen-setup-instruction "1.4.0"))
(package-invoke 'magit)
;;; }}}

;;; Yasnippet Settings {{{
(package-config 'yasnippet		; Extension: yasnippet
	(setq yas-snippet-dirs (list (concat user-emacs-top-directory "yasnippets/"))))
;;; }}}

;;; Projectile Settings {{{
(package-config 'projectile		; Extension: projectile
	(setq projectile-mode-line (format " Proj[%s]" (projectile-project-name))))
(package-invoke 'projectile-mode nil 'projectile)
;;; }}}

;;; Auto-Complete Settings {{{
(package-config 'auto-complete		; Extension: auto-complete
	;;(add-to-list 'ac-dictionary-directories (concat user-emacs-directory "acdict/"))
	(require 'auto-complete-config)
	(ac-config-default)
	(setq ac-auto-show-menu nil)
	(setq ac-use-quick-help nil)
	;;(define-key ac-mode-map "\M-/" 'auto-complete)
	)
;;; }}}

;;; Web-mode Settings {{{
(package-config 'web-mode		; Extension: web-mode
	(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
	(setq web-mode-code-indent-offset 4)
	(setq web-mode-markup-indent-offset 4)
	(setq web-mode-style-padding 4))
;;; }}}

;;; JS2-mode Settings {{{
(package-config 'js2-mode		; Extension: js2-mode
	(add-to-list 'auto-mode-alist '("\\.html?\\'" . js2-mode))
	;;(add-hook 'js-mode-hook 'init/company-tern-enable)
	(package-depend 'lsp-mode
		(add-hook 'js2-mode-hook 'init/lsp-javascript-enable)))
;;; }}}

;;; Python-mode Settings {{{
(package-config 'python		; Extension: python-mode
	(defun init/setting-python-mode ()
		(setq indent-tabs-mode t)
		(setq python-indent-offset 4)
		(setq tab-width 4))
	(add-hook 'python-mode-hook 'init/setting-python-mode)
	(package-depend 'lsp-mode
		(add-hook 'python-mode-hook 'init/lsp-python-enable))
	;;(add-hook 'python-mode-hook 'init/company-jedi-enable)
	)
;;; }}}

;;; Jinja2 Settings {{{
(package-config 'jinja2-mode
	(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . jinja2-mode)))
;;; }}}

;;; Elpy Settings {{{
(package-config 'elpy		; Extension: elpy
	(setq elpy-modules (delete 'elpy-module-highlight-indentation elpy-modules)))
;;; }}}

;;; Undo-Tree Settings {{{
(package-config 'undo-tree		; Extension: undo-tree
	(setq undo-tree-mode-lighter ""))
;;; }}}

;;; Mozc Settings {{{
(package-invoke 'mozc)
;;; }}}

;;; Skk Settings {{{
(setq skk-user-directory (concat user-emacs-directory "ddskk/"))
(package-config 'skk		; Extension: SKK
	;;(define-key global-map "\C-x\C-j" 'skk-mode)
	(setq skk-henkan-show-candidates-keys (list ?a ?o ?e ?u ?h ?t ?n ?s))
	(setq skk-indicator-use-cursor-color nil)
	(setq skk-kakutei-key (kbd "C-t"))
	(setq skk-background-mode 'dark) ; For DDSKK 15.1 Hot-Fix
	(require 'skk-hint nil t)
	(when (require 'skk-search-web nil t)
		;;(add-to-list 'skk-search-prog-list '(skk-search-web 'skk-google-cgi-api-for-japanese-input) t)
		(add-to-list 'skk-search-prog-list '(skk-search-web 'skk-google-suggest) t)))
(package-invoke 'skk-preload 'text-mode-hook 'ddskk)
;;; }}}

;;; w3m Settings {{{
;(package-config 'w3m-filter		; Extension: w3m
	;;(defun w3m-filter-alc-lay (url) (w3m-filter-delete-regions url "<!-- interest_match_relevant_zone_start -->" "<!-- ▲ 英辞郎ヘッダ ▲ -->"))
;	(defun w3m-filter-alc-lay (url) (w3m-filter-delete-regions url "<!-- interest_match_relevant_zone_start -->" "<!-- ▼ 検索結果本体 ▼ -->"))
;	(add-to-list 'w3m-filter-configuration '(t "Suck!" "\\`http://eow\\.alc\\.co\\.jp/search" w3m-filter-alc-lay)))
;;; }}}

;;; Whitespace Settings {{{
(package-config 'whitespace		; Extension: whitespace
	(setq whitespace-style '(face tabs trailing space-before-tab empty tab-mark)))
(package-invoke 'whitespace-mode 'prog-mode-hook)
(defface dspace-emphasis '((t :background "red")) "Used for dspace emphasis")
(defun init/emphasis-dspace ()
	(font-lock-add-keywords nil '(("　" . init/dspace-emphasis))))
(font-lock-add-keywords 'lisp-mode '(("b" . (0 highlight t t))))
;;; }}}

;;; Xah-math-input-mode Settings {{{
(package-config 'xah-math-input
	(puthash "wb" "◦" xah-math-input-abrvs))
;;; }}}

;;; Uniquify Settings {{{
(package-config 'uniquify
	(setq uniquify-buffer-name-style 'post-forward-angle-brackets))
(package-invoke 'uniquify)
;;; }}}

;;; Extension }}}

;;; Tramp Settings {{{
;; A workaround for Tramp for text corruption.
;;(setq tramp-remote-process-environment (quote ("HISTFILE=$HOME/.tramp_history" "HISTSIZE=1" "LC_MESSAGE=C" "TERM=dumb" "EMACS=t" "INSIDE_EMACS='24.3.1,tramp:2.2.6-24.3'" "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH=" "PAGER=\"\"" "autocorrect=" "correct=")))
;;; }}}

;;; Terminal Emulator Settings {{{
;;(when (require 'term+ nil t)
;;(require 'xterm-256color nil t))
;;; }}}

;;; Dired Settings {{{
(package-config 'dired
	(setq dired-listing-switches "-alh")
	(setq dired-dwim-target t))
;;; }}}

;;; Ediff Settiings {{{
(package-config 'ediff
	(setq ediff-window-setup-function 'ediff-setup-windows-plain))
;;; }}}

;;; Spacemacs {{{
(setenv "SPACEMACSDIR" "/tmp/spacemacsconf")
(setq spacemacs-start-directory (concat user-emacs-top-directory "spacemacs/"))
(setq-default dotspacemacs-themes '(deeper-blue))
(setq-default dotspacemacs-default-font '("Inconsolata"
                                          :size 24
                                          :weight normal
                                          :width normal
                                          :powerline-scale 1.1))
;(ext-config "spacemacs/init.el")
;;; }}}

;;; Customize Settings {{{
(setq custom-file (concat user-emacs-top-directory "customize.el"))
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
;;; End:
