;;; -*- mode: Emacs-Lisp -*- ;;;
;;; font.el --- A emacs external setting file (fontset definition)

;; Make fontsets based on ascii
(let* ((fontset-name "Akadori")
       (size 12)
       (asciifont "Inconsolata")
       (jpfont "VL Gothic")
       (failfont "IPAGothic")
       (weight "normal")
       (slant "normal")
       (afont (format "%s-%d:weight=%s:slant=%s" asciifont size weight slant))
       (afontspec (font-spec :family asciifont))
       (jfontspec (font-spec :family jpfont))
       (ffontspec (font-spec :family failfont))
       (fsn (create-fontset-from-ascii-font afont nil fontset-name)))
  (set-fontset-font fsn 'unicode jfontspec nil 'append)
  (set-fontset-font fsn 'unicode ffontspec nil 'append)
)

(let* ((fontset-name "Aoshima")
       (asciifont "Inconsolata")
       (size 16)
       (weight "normal")
       (slant "normal")
       (fsn (create-fontset-from-ascii-font (format "%s-%d:weight=%s:slant=%s" asciifont size weight slant) nil fontset-name)))
  (set-fontset-font fsn 'unicode (font-spec :family "VL Gothic") nil 'append)
  (set-fontset-font fsn 'unicode (font-spec :family "IPAGothic") nil 'append)
  (set-fontset-font fsn 'unicode (font-spec :family "IPAexGothic") nil 'append)
  (set-fontset-font fsn 'unicode (font-spec :family "IPAmjMincho") nil 'append))
       

;; Rescaling
(dolist
    (elt '(("^-apple-hiragino.*" . 1.2)
	   (".*osaka-bold.*" . 1.2)
	   (".*osaka-medium.*" . 1.2)
	   (".*courier-bold-.*-mac-roman" . 1.0)
	   (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
	   (".*monaco-bold-.*-mac-roman" . 0.9)))
  (add-to-list 'face-font-rescale-alist elt))
