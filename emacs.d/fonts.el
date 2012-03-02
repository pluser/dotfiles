;;; This is Fontset Definition

(let* ((fontset-name "Inconsolata")
       (size 14)
       (asciifont "Inconsolata")
       (weight "normal")
       (slant "normal")
       (font (format "%s-%d:weight=%s:slant=%s" asciifont size weight slant))
       (fontspec (font-spec :family asciifont))
       (fsn (create-fontset-from-ascii-font font nil fontset-name))))
