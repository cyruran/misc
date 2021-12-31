;; FSVN
(if (member 'fsvn features)
    (require 'fsvn)

  (setq my-fsvn-additional nil)
  (when my-fsvn-additional
    (defun fsvn-log-list-diff-with-region-summarize (args)
      (let* ((revs (fsvn-log-list-region-revision t))
             (new (car revs))
             (old (cdr revs)))
        (async-shell-command (format "svn diff --old %s --new %s --summarize" old new))))

    (defun fsvn-log-list-diff-generic-summarize (&optional args)
      "Diff current revision at point following cases.
When mark is activated diff for region terminated revisions.
Otherwise diff at point revision with working copy file or directory.
"
      (interactive (fsvn-logview-cmd-read-diff-args))
      (cond
       (mark-active
        (fsvn-log-list-diff-with-region-summarize args))
       ((fsvn-url-repository-p fsvn-logview-target-urlrev)
        (error "This buffer has non working copy"))
       (t
        (fsvn-log-list-diff-with-wc args)))))

  (defun my-fsvn-this-file-log ()
    (interactive)
    (my-dired-select-this-file)
    (when (string-equal major-mode "fsvn-browse-mode")
      (call-interactively 'fsvn-browse-logview-this)))

  (global-set-key (kbd "C-o z l") 'my-fsvn-this-file-log)

  (provide 'fsvn-custom))
