;;; ox-anki2.el --- Anki Back-end for Org Export Engine

(require 'ox)

(org-export-define-backend 'anki
  '(
    (headline . org-anki-headline)
    ;(section . org-anki-section)
    )
  :menu-entry
  '(?a "Export to Anki"
       ((?b "As tab separated Buffer" org-anki-export-to-buffer))))

(defun org-anki-headline (headline contents info)
  "headline"
  (pp-to-string (cdr (car (last headline)))))
;  (concat (org-element-property :raw-value headline)))

(defun org-anki-section (section contents info)
  "section")
  ;(concat contents (org-element-property :raw-value section)))

(defun org-anki-export-to-buffer (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'anki "*Org Anki Export*" async subtreep visible-only body-only ext-plist 'fundamental-mode))
;  (org-export-to-buffer 'anki "*Org Anki Export*" async subtreep visible-only 'body-only ext-plist 'fundamental-mode))
