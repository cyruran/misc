(defvar jcl-mode-hook nil)

(defvar jcl-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'jcl-newline)
    map)
  "Keymap for JCL major mode")

(add-to-list 'auto-mode-alist '("\\.[Jj][Cc][Ll]'" . jcl-mode))

(defconst jcl-font-lock-keywords-1
  (list
   '("^//\\*.*$" . font-lock-comment-face)
   '("\\<\\(CNTL\\|DD\\|ENDCNTL\\|EXEC\\|IF\\|THEN\\|ELSE\\|ENDIF\\|INCLUDE\\|JCLLIB\\|JOB\\|OUTPUT\\|PEND\\|PROC\\|SET\\|XMIT\\)\\>" . font-lock-keyword-face)
   '("^[^/][^/].*$" . font-lock-string-face)
   '("^//\\(\\S-+\\)?" . font-lock-variable-name-face)
   '("[A-Z][A-Z0-9]*?=" . font-lock-type-face)
   '("&&?[A-Z][A-Z0-9]*" . font-lock-preprocessor-face)
   '("'[^']*'" . font-lock-string-face)))

(defvar jcl-font-lock-keywords jcl-font-lock-keywords-1)

(defun jcl-newline ()
  "Indent current line as JCL code"
  (interactive)
  (newline)
  (if (save-excursion
        (forward-line -1)
        (or
         (looking-at "/\\*")
         (and (looking-at "^//")
             (not (or
                   (looking-at "//.*DD.*\\<DATA\\>.*\\<DLM=")
                   (looking-at "//.*DD\\s-+\\*"))))))
      (progn
        (insert "//"))))

(defun jcl-mode ()
  "Major JCL mode"
  (interactive)
  (kill-all-local-variables)
  (use-local-map jcl-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(jcl-font-lock-keywords))
  (ruler-mode)
  (setq major-mode 'jcl-mode)
  (setq mode-name "JCL")
  (run-hooks 'jcl-mode-hook))

(provide 'jcl-mode)
