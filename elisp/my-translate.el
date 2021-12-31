(provide 'google-translate-default-ui)

(defun enru-translate-query (text)
  (interactive "MEnter text")
  (google-translate-translate "en" "ru" text))
