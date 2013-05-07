;;; This is Fontset Definition

;; Make fontsets based on ascii
(let* ((fontset-name "Aoshima")
       (size 11)
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

;; Rescaling
(dolist
    (elt '(("^-apple-hiragino.*" . 1.2)
	   (".*osaka-bold.*" . 1.2)
	   (".*osaka-medium.*" . 1.2)
	   (".*courier-bold-.*-mac-roman" . 1.0)
	   (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
	   (".*monaco-bold-.*-mac-roman" . 0.9)))
  (add-to-list 'face-font-rescale-alist elt))
