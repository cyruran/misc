(defun org-word-export (form)
  (async-shell-command (format "libreoffice --headless --convert-to %s %s" form (org-odt-export-to-odt))))

(defun org-word-export-to-doc ()
  (interactive)
  (org-word-export "doc"))

(defun org-word-export-to-docx ()
  (interactive)
  (org-word-export "docx"))

(provide 'org-word-export)
