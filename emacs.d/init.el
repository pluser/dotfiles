;;; init.el --- A emacs setting file
;;; Commentary:
;;;  Heavily personalized Emacs configuration file
;;; Code:

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
					(add-to-list 'exec-path path)))
		(when (file-directory-p path-list)
			(add-to-list 'exec-path path-list))))
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
	(if (< emacs-major-version 24)
		(progn (set-face-font 'default "fontset-mejiro")
					 (set-frame-font "fontset-kawasemi"))
		(progn (add-to-list 'default-frame-alist '(font . "fontset-mejiro"))
					 (set-face-attribute 'variable-pitch nil :fontset "fontset-kawasemi")
					 (set-face-attribute 'fixed-pitch nil :fontset "fontset-mejiro"))))
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
	(set-variable 'package-user-dir (init/locate-user-config "packages/"))
	(package-initialize))
;	(dolist (pkg init/autoinstall-packages)
;		(unless (package-installed-p pkg)
;			(unless package-archive-contents (package-refresh-contents))
;			(package-install pkg))))
(package-invoke 'package)
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
(if (fboundp 'load-theme)
		(load-theme 'deeper-blue)
	(package-config 'color-theme		; Extension: color-theme
		(color-theme-initialize)
		(color-theme-deep-blue))
	(package-invoke 'color-theme))
;;; }}}

;;; File Opener {{{
(package-config 'filecache
	(file-cache-add-file (init/locate-user-config "init.el")))
(package-invoke 'filecache)
(package-config 'recentf
	(set-variable 'recentf-max-saved-items 2000))
(package-invoke 'recentf-mode)
;;; }}}

;;; Diff Setting {{{
(package-config 'diff)
;;; }}}

;;; Code ornament / Visual {{{
(show-paren-mode t)
(set-variable 'show-paren-style 'mixed)
(set-variable 'scroll-conservatively 1) ; amount of scrolling
;(set-variable 'scroll-margin 5) ; keep lines on scrolling
(set-variable 'line-number-mode t) ; show line number at modeline
(set-variable 'column-number-mode t) ; show column number at modeline
(setq-default truncate-lines nil)
(set-variable 'auto-hscroll-mode 'current-line) ; scroll one line or whole screen
(when (< emacs-major-version 26) (global-linum-mode t)) ; show line number at left margin
(setq-default tab-width 4)
(global-hl-line-mode t) ; highlight a line cursor is on
(defun init/prog-mode-editor-style ()
	"Settings for 'prog-mode."
	(set-variable 'display-line-numbers 'relative))
(add-hook 'prog-mode-hook 'init/prog-mode-editor-style)
(defface dspace-emphasis '((t :background "red")) "Used for dspace emphasis")
(defun init/emphasis-dspace ()
	"Emphasis ideographic space."
	(font-lock-add-keywords nil '(("　" 0 'dspace-emphasis t))))
(add-hook 'prog-mode-hook 'init/emphasis-dspace)

;;; }}}

;;; Code styling {{{
(setq-default indent-tabs-mode t)
(setq-default c-basic-offset 4)
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
(package-config 'savehist
	(set-variable 'history-length 100000))
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
(package-config 'files
	(add-to-list 'safe-local-variable-values '(origami-fold-style . triple-braces))
	(add-to-list 'safe-local-eval-forms '(outline-minor-mode t)))
;;; }}}


;;; Major Mode {{{

;;; Web-mode Settings {{{
(package-config 'web-mode		; Extension: web-mode
	(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
	(set-variable 'web-mode-code-indent-offset 4)
	(set-variable 'web-mode-markup-indent-offset 4)
	(set-variable 'web-mode-style-padding 4))
;;; }}}

;;; JS2-mode Settings {{{
(package-config 'js2-mode		; Extension: js2-mode
	(add-to-list 'auto-mode-alist '("\\.html?\\'" . js2-mode))
	;;(add-hook 'js-mode-hook 'init/company-tern-enable)
	(package-depend 'lsp-mode
		(add-hook 'js2-mode-hook 'lsp-jts-enable)))
;;; }}}

;;; Python-mode Settings {{{
(package-config 'python		; Extension: python-mode
	(defun init/setting-python-mode ()
		(setq indent-tabs-mode t)
		(setq python-indent-offset 4)
		(setq tab-width 4))
	(add-hook 'python-mode-hook 'init/setting-python-mode)
	(package-depend 'lsp-mode
		(add-hook 'python-mode-hook 'lsp-pyls-enable))
	;;(add-hook 'python-mode-hook 'init/company-jedi-enable)
	)
;;; }}}

;;; CC-Mode Settings {{{
(package-config 'cc-mode
	(package-depend 'ensime-mode
		(add-hook 'java-mode-hook 'ensime-mode))
	(package-depend 'lsp-mode
		(package-depend 'cquery
			(add-hook 'c-mode-hook 'lsp-cquery-enable)))
	(package-invoke 'cquery))
;;; }}}

;;; Jinja2 Settings {{{
(package-config 'jinja2-mode
	(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . jinja2-mode)))
;;; }}}

;;; Major Mode }}}

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
	(set-variable 'TeX-engine 'luatex)
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
	(latex-math-preview-define-convert-function 'convert))
;;; }}}

;;; Org-mode Settings {{{
(package-config 'org		; Extension: org
	;;(set-variable 'org-babel-no-eval-on-ctrl-c-ctrl-c t)
	(set-variable 'org-startup-truncated nil)
	(org-babel-do-load-languages
	 'org-babel-load-languages
	 '((emacs-lisp . t) (dot . t)))
	(string-match "\\\\documentclass{article}" org-format-latex-header)
	(set-variable 'org-format-latex-header (replace-match "\\documentclass[autodetect-engine,dvipdfmx-if-dvi,ja=standard,enablejfam=true]{bxjsarticle}" t t org-format-latex-header))
	(add-to-list 'org-preview-latex-process-alist '(xelatex-dvisvgm :programs ("xelatex" "dvisvgm") :description "xdv > svg" :message "you need to install the programs: xelatex and dvisvgm." :use-xcolor t :image-input-type "xdv" :image-output-type "svg" :image-size-adjust (2.0 . 2.0) :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f") :image-converter ("dvisvgm %f -n -b min -c %S -o %O")))
	(set-variable 'org-preview-latex-default-process 'xelatex-dvisvgm)
	(package-config 'ox
		(set-variable 'org-export-allow-bind-keywords t))
	(package-config 'ox-pandoc
		(set-variable 'org-pandoc-options-for-latex-pdf '((pdf-engine . "lualatex"))))
	(package-config 'ox-latex
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
(package-config 'helm-mode		; Extension: helm
	(package-invoke 'helm-config 'require-only)
	(define-key global-map (kbd "C-x b") 'helm-buffers-list)
	(define-key global-map (kbd "C-x f") 'helm-find-files)
	(define-key global-map (kbd "M-x") 'helm-M-x)
	(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
	(define-key helm-map (kbd "C-z") 'helm-select-action)
	(set-variable 'helm-autoresize-max-height 80)
	(set-variable 'helm-autoresize-mode t))
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
(package-invoke 'evil-mode 'prog-mode-hook 'evil)
(package-invoke 'evil-surround 'evil-mode-hook)
;;; }}}

;;; Company Settings {{{
(package-config 'company		; Extension: company
	(set-variable 'company-transformers '(company-sort-by-backend-importance company-sort-by-occurrence))
	(package-config 'company-statistics		; Extension: company-statistics
		(add-to-list 'company-transformars 'company-sort-by-statistics))
	(set-variable 'company-frontends '(company-pseudo-tooltip-unless-just-one-frontend-with-delay company-echo-metadata-frontend company-preview-frontend))
	;;(define-key global-map (kbd "C-t") 'company-complete)
	;;(define-key company-active-map 'company-complete-common nil)
	(define-key company-mode-map (kbd "C-t") 'company-complete-common)
	(set-variable 'company-idle-delay 0.5)
	(set-variable 'company-tooltip-idle-delay 0.5)
	(set-variable 'company-selection-wrap-around t)
	(set-variable 'company-show-numbers t)
	(package-config 'company-math		; Extension: company-math
		(add-to-list 'company-backends 'company-math-symbols-unicode))
	(package-config 'company-tern		; Extension: company-tern
		(set-variable 'company-tern-property-marker nil))
	(defun init/company-lsp-enable ()
		(make-local-variable 'company-backends)
		(add-to-list 'company-backends 'company-lsp))
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
(package-config 'lsp-mode		; Extension: lsp-mode
	(package-invoke 'lsp-ui-mode 'lsp-mode-hook)
	(lsp-define-stdio-client lsp-pyls "python" (lambda () default-directory) '("pyls"))
	(lsp-define-stdio-client lsp-jts "javascript" (lambda () default-directory) '("javascript-typescript-stdio"))
	(lsp-define-stdio-client lsp-clangd "c" (lambda () default-directory) '("clangd")))
(package-config 'lsp-ui		; Extension: lsp-ui
	(set-variable 'lsp-ui-sideline-enable nil))
(package-invoke 'lsp-mode 'require-only)
;;; }}}

;;; Company-LSP Settings {{{		; Extension: company-lsp
(defun init/enable-company-lsp ()
	(package-depend 'company
		(add-to-list 'company-backends 'company-lsp)))
(add-hook 'lsp-mode-hook 'init/enable-company-lsp)
;;; }}}

;;; Magit Settings {{{
(package-config 'magit		; Extension: magit
	(define-key global-map (kbd "<f12>") 'magit-status)
	(set-variable 'magit-last-seen-setup-instruction "1.4.0"))
(package-invoke 'magit)
;;; }}}

;;; Yasnippet Settings {{{
(package-config 'yasnippet		; Extension: yasnippet
	(set-variable 'yas-snippet-dirs (list (init/locate-user-config "yasnippets/"))))
(package-invoke 'yas-minor-mode 'prog-mode-hook 'yasnippet)
(package-invoke 'yas-minor-mode 'org-mode-hook 'yasnippet)
;;; }}}

;;; Projectile Settings {{{
(package-config 'projectile		; Extension: projectile
	(set-variable 'projectile-mode-line (format " Proj[%s]" (projectile-project-name))))
(package-invoke 'projectile-mode nil 'projectile)
;;; }}}

;;; Auto-Complete Settings {{{
(package-config 'auto-complete		; Extension: auto-complete
	;;(add-to-list 'ac-dictionary-directories (locate-user-emacs-file "acdict/"))
	(require 'auto-complete-config)
	(ac-config-default)
	(set-variable 'ac-auto-show-menu nil)
	(set-variable 'ac-use-quick-help nil)
	;;(define-key ac-mode-map "\M-/" 'auto-complete)
	)
;;; }}}

;;; Elpy Settings {{{
(package-config 'elpy		; Extension: elpy
	(set-variable 'elpy-modules (delete 'elpy-module-highlight-indentation elpy-modules)))
;;; }}}

;;; Undo-Tree Settings {{{
(package-config 'undo-tree		; Extension: undo-tree
	(set-variable 'undo-tree-mode-lighter ""))
;;; }}}

;;; Mozc Settings {{{
(package-config 'mozc		; Extension: mozc
	(set-variable 'mozc-candidate-style 'echo-area))
(package-invoke 'mozc)
;;; }}}

;;; Skk Settings {{{
(setq skk-user-directory (locate-user-emacs-file "ddskk/"))
(package-config 'skk		; Extension: SKK
	;;(define-key global-map "\C-x\C-j" 'skk-mode)
	(set-variable 'skk-henkan-show-candidates-keys (list ?a ?o ?e ?u ?h ?t ?n ?s))
	(set-variable 'skk-indicator-use-cursor-color nil)
	(set-variable 'skk-kakutei-key (kbd "C-t"))
	(set-variable 'skk-use-auto-kutouten t)
	(set-variable 'skk-background-mode 'dark) ; For DDSKK 15.1 Hot-Fix
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
	(set-variable 'whitespace-style '(face tabs trailing space-before-tab empty tab-mark)))
(package-invoke 'whitespace-mode 'prog-mode-hook)
;;; }}}

;;; Xah-math-input-mode Settings {{{
(package-config 'xah-math-input
	(puthash "wb" "◦" xah-math-input-abrvs))
;;; }}}

;;; Uniquify Settings {{{
(package-config 'uniquify
	(set-variable 'uniquify-buffer-name-style 'post-forward-angle-brackets))
(package-invoke 'uniquify)
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
(package-config 'dired
	(set-variable 'dired-listing-switches "-alh")
	(set-variable 'dired-dwim-target t))
;;; }}}

;;; Ediff Settiings {{{
(package-config 'ediff
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
