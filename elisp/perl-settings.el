;; Perl
;; (load "~/.emacs.d/custom/perlysense.el")

;; Indentation settings
(setq cperl-extra-newline-before-brace t
      cperl-brace-offset              -2
      cperl-merge-trailing-else        nil
      cperl-indent-level               4
      tab-width                        4
      default-tab-width                4
      indent-tabs-mode                 nil)

(setq perl-var-definition-regexp "^\\s-*\\(my\\|our\\|local\\)\\(\\s-+\\|\\s-*([^)]*\\)%s\\>")

(defun perltidy-region ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "perltidy" nil t))

(defun perltidy-buffer ()
  (interactive)
  (let ((position (point)))
    (shell-command-on-region (point-min) (point-max) "perltidy" nil t)
    (goto-char position)))

(defun my-perl-terse ()
  (interactive)
  (shell-command-on-file (buffer-file-name) "perl -MO=Terse"))

(defun my-perl-xref ()
  (interactive)
  (shell-command-on-file (buffer-file-name) "perl -MO=Xref"))

(defun my-perl-projectile-etags ()
  (interactive)
  (cd (projectile-project-root))
  (async-shell-command "etags --language=perl `find -type f -not -name '*.*' -not -path './.git/*' -not -path './.svn/*' -o -name '*.p[lm]'`"))

(defun cperl-search-decl ()
  (interactive)
  (let* ((var (cperl-word-at-point))
         (ending (substring var -1)))
    (concat (cond ((eq ending "[") "@")
                  ((eq ending "{") "%"))
            (replace-regexp-in-string "^$\\(.*?\\)\\s-*" "\1" var))
    (search-backward-regexp
     (format perl-var-definition-regexp var))))

(defun cperl-occur-regex-function (regex)
  (let ((last (point)))
    (narrow-to-defun)
    (occur regex)
    (widen)
    (goto-char last)))

(defun perl-projectile-grep (regex)
  (rgrep regex "*.p[lm] [^.]*" (projectile-project-root)))

(defun perl-sub-usages ()
  (interactive)
  (let ((sub (replace-regexp-in-string "^.*::" "" (which-function))))
    (projectile-grep (format "\\(\\<\\|::\\|->\\)%s\\>" sub))))

(defun perl-filename-usage ()
  (interactive)
  (my-projectile-grep (format "%s\\>" (file-name-nondirectory (buffer-file-name)))))

(defun perl-get-var-name-type (variable)
  (let* ((last (substring variable -1 nil))
         (first (substring variable 0 1))
         (type (if (string= first "$")
                   (cond ((string= last "[") "@")
                         ((string= last "{") "%")
                         (t first))
                 (if (string-match first "[$@%]") first nil)))
         (bare (replace-regexp-in-string "^[@$%]?\\(.*?\\)\\s-*[\\[\\{]?$" "\\1" variable)))
    (cons bare type)))

(defun perl-var-occurences-regex (variable)
  (let* ((info (perl-get-var-name-type variable))
         (name (car info))
         (type (cdr info)))
    (concat type name (cond ((string= type "@") (concat "\\|$" name "\\s-*\\["))
                            ((string= type "%") (concat "\\|$" name "\\s-*\\{"))))))

(defun cperl-occur-var-function ()
  (interactive)
  (narrow-to-defun)
  (occur (perl-var-occurences-regex (cperl-word-at-point)))
  (widen))

(defun cperl-occur-word-function ()
  (interactive)
  (cperl-occur-regex-function (format "\\<%s\\>" (cperl-word-at-point))))

(defun perl-sub-usages-current-file ()
  (interactive)
  (occur (format "%s(" (which-function))))

(defun perl-compile ()
  (interactive)
  (shell-command (format "perl -c %s" (buffer-file-name))))

;; REPL
;; apt install libdevel-repl-perl

(defun perl-start-repl ()
  (interactive)
  (or (get-buffer "*re.pl*")
      (let ((repl (ansi-term "/bin/bash" "re.pl")))
        (send-string repl "re.pl\n")
        repl)))

(defun perl-send-region-repl ()
  (interactive)
  (send-region (perl-start-repl) (region-beginning) (region-end)))

(defun perl-send-buffer-perl ()
  (interactive)
  (send-region (perl-start-repl) (buffer-end 0) (buffer-end 1)))

;; Perl keys
(eval-after-load 'cperl-mode
  '(progn (define-key cperl-mode-map (kbd "C-o v s") 'cperl-search-decl)
    (define-key cperl-mode-map (kbd "C-o v o") (lambda () (interactive) (occur (format "%s\\>" (cperl-word-at-point)))))
    (define-key cperl-mode-map (kbd "C-o v f o") 'cperl-occur-var-function)
    (define-key cperl-mode-map (kbd "C-o w o") (lambda () (interactive) (occur (format "\\<%s\\>" (cperl-word-at-point)))))
    (define-key cperl-mode-map (kbd "C-o w f o") 'cperl-occur-word-function)
    (define-key cperl-mode-map (kbd "C-o w f") (lambda () (interactive) (perl-projectile-grep (cperl-word-at-point))))
    (define-key cperl-mode-map (kbd "C-o c f") (lambda () (interactive) (kill-new (which-function))))
    (define-key cperl-mode-map (kbd "C-o f c") 'perl-sub-usages)
    (define-key cperl-mode-map (kbd "C-o f f u") 'perl-filename-usage)

    (define-key cperl-mode-map (kbd "C-o f a") 'my-projectile-grep)

    (define-key cperl-mode-map (kbd "C-o e t") 'my-perl-projectile-etags)

    (define-key cperl-mode-map (kbd "C-o p t r") 'perltidy-region)
    (define-key cperl-mode-map (kbd "C-o p t b") 'perltidy-buffer)

    (define-key cperl-mode-map (kbd "<f9> r") 'perltidy-region)
    (define-key cperl-mode-map (kbd "<f9> b") 'perltidy-buffer)

    (define-key cperl-mode-map (kbd "M-<f5>") (lambda () (interactive) (cperl-mode)))
    (define-key cperl-mode-map (kbd "C-c C-c") (lambda () (interactive) (cperl-mode)))))

(add-hook 'cperl-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "perl %s" (buffer-file-name)))))

(provide 'perl-settings)
