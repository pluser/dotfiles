;;; -*- lexical-binding: t; -*-
;;; init.el --- A emacs setting file
;;; Commentary:
;;;  Heavily personalized Emacs configuration file
;;; Code:

;;; Debug settings {{{
(setq debug-on-error t)
(setq warning-minimum-level :error)
;;; }}}

;;; Minimize garbage collection during startup {{{
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 800000)))
;;; }}}

;;; Font settings {{{
(when (display-graphic-p)
	(set-face-attribute 'default nil :family "Inconsolata")
	(set-fontset-font nil 'cp932 (font-spec :family "Migu 1M")))
;(add-to-list 'default-frame-alist '(font . "Inconsolata-16"))
;;; }}}

;;; Package settings {{{
(defvar bootstrap-version)
;;(let ((bootstrap-file
;;       (expand-file-name
;;        "straight/repos/straight.el/bootstrap.el"
;;        (or (bound-and-true-p straight-base-dir)
;;            user-emacs-directory)))
;;      (bootstrap-version 7))
;;  (unless (file-exists-p bootstrap-file)
;;    (with-current-buffer
;;        (url-retrieve-synchronously
;;         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;         'silent 'inhibit-cookies)
;;      (goto-char (point-max))
;;      (eval-print-last-sexp)))
;;  (load bootstrap-file nil 'nomessage))

;;(straight-use-package 'use-package)
;;(use-package straight
;;	:custom
;;	;;(use-package-always-defer t)
;;	(use-package-verbose t)
;;	(use-package-compute-statistics t)
;;	(use-package-always-ensure t)
;;	(use-package-always-demand t)
;;	(straight-use-package-by-default t))
;;; }}}

(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;(use-package seq) ;; FIXME: workaround the bug of elpaca. remove it.

;;; Visual / Theme settings {{{
(use-package doom-themes
	:config
	(load-theme 'doom-city-lights t)
	(doom-themes-visual-bell-config)
	(doom-themes-org-config))
(transient-mark-mode 0)
(set-variable 'visible-bell t)
(set-variable 'ring-bell-function 'ignore)
(set-variable 'use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)
(tool-bar-mode 0)
(menu-bar-mode 0)
(show-paren-mode t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq indicate-empty-lines t)
(setq auto-hscroll-mode 'current-line)
(global-hl-line-mode t)
(setq display-line-numbers-type 'relative)
;;; }}}

;;; Language / file encoding settings {{{
(set-language-environment "Japanese")
(set-variable 'default-input-method "japanese-skk")
(prefer-coding-system 'utf-8)
(defun my/replace-kutouten-from-ja-to-en ()
  (interactive)
  (format-replace-strings '(("、" . "，") ("。" . "．"))))
(defun my/replace-kutouten-from-en-to-ja ()
  (interactive)
  (format-replace-strings '(("，" . "、") ("．" . "。"))))
;;; }}}

;;; (default) Coding style settings {{{
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq c-tab-always-indent 'indent)
;;; }}}

;;; Global key bindings {{{
(define-key global-map (kbd "C-h") 'left-char)
(define-key global-map (kbd "C-t") 'next-line)
(define-key global-map (kbd "C-n") 'previous-line)
(define-key global-map (kbd "C-s") 'right-char)
;;; }}}

;;; Modeline settings {{{
(use-package doom-modeline
	:config
	(doom-modeline-mode t))
;;; }}}

;;; EditorConfig settings {{{
(use-package editorconfig
	:config
	(editorconfig-mode t))
;;; }}}

;;; Completion UI settings {{{
(use-package vertico
	:config
	(vertico-mode t))
(use-package orderless
	:config
	(setq completion-styles '(orderless)))
(use-package marginalia
	:config
	(marginalia-mode t))
(use-package consult
	:after recentf
	:config
	(global-set-key (kbd "C-x b") 'consult-buffer)
	(global-set-key (kbd "C-x C-b") 'consult-buffer)
	(global-set-key (kbd "C-x C-f") 'consult-find)
	(global-set-key (kbd "C-x C-r") 'consult-recent-file)
	(global-set-key (kbd "M-y") 'consult-yank-pop)
	(global-set-key (kbd "C-M-l") 'consult-line)
	(global-set-key (kbd "C-M-j") 'consult-imenu)
	(global-set-key (kbd "C-M-k") 'consult-ripgrep)
	(global-set-key (kbd "C-M-o") 'consult-outline)
	(global-set-key (kbd "C-M-t") 'consult-theme)
	(global-set-key (kbd "C-M-b") 'consult-bookmark)
	(global-set-key (kbd "C-M-m") 'consult-mark)
	(global-set-key (kbd "C-M-s") 'consult-isearch)
	(global-set-key (kbd "C-M-S") 'consult-line-multi)
	(global-set-key [remap goto-line] 'consult-goto-line))
(use-package embark
	:bind
	(("C-." . embark-act)
	("C-;" . embark-dwim)
	("C-B" . embark-bindings)))
(use-package embark-consult
	:after (embark consult)
	:hook
	(embark-collect-mode . consult-preview-at-point-mode))
;;; }}}

;;; Completion settings {{{
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-preselect 'prompt)
  :config
  (global-corfu-mode t))
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (plist-put kind-icon-default-style :scale 0.8))
(use-package nerd-icons :demand)
;;; }}}

;;; Syntax settings {{{
(use-package treesit
  :elpaca nil
  :custom
  (treesit-font-lock-level 4))
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
;;; }}}

;;; Modal edit {{{
(use-package meow)
;;; }}}

(use-package which-key
	:hook (emacs-startup . which-key-mode)
	:config
	(which-key-setup-side-window-right-bottom))
(use-package lsp-mode
	:custom
	(lsp-keymap-prefix "C-d")
	(lsp-completion-provider :none)
	:commands lsp
	:hook
	(lsp-mode . lsp-enable-which-key-integration)
	(rust-mode . lsp))
(use-package dap-mode
  :defer t
  :after lsp-mode)
(use-package projectile)
(use-package magit
	:defer t
	:bind ("<f12>" . magit-status))
(use-package treemacs)
(use-package helpful
	:defer t
	:bind
	([remap describe-function] . #'helpful-callable)
	([remap describe-variable] . #'helpful-variable)
	([remap describe-command] . #'helpful-command)
	([remap describe-key] . #'helpful-key))
(use-package recentf
	:elpaca nil
	:config
	(recentf-mode t))
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(seq)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; Local Variables:
;;; mode: Emacs-Lisp
;;; coding: utf-8
;;; lisp-body-indent: 2
;;; tab-width: 2
;;; End:
