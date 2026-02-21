;;; custom-help-quick.el --- Air Emacs config  -*- lexical-binding: t; -*-

;; Value should be a list of elements, each element should of the form
;;   (GROUP-NAME (COMMAND . DESCRIPTION) (COMMAND . DESCRIPTION)...)

(setq help-quick-sections
      '(("File" (save-buffers-kill-terminal . "exit") (find-file . "find")
         (save-buffer . "save")
         (write-file . "as..")
         (save-some-buffers . "all"))
        ("Buffer" (kill-buffer . "kill") (list-buffers . "list")
         (switch-to-buffer . "switch") (goto-line . "goto line")
         (read-only-mode . "readonly"))
        ("Window" (enlarge-window . "enlarge V") (shrink-window . "shrink  V")
         (enlarge-window-horizontally . "enlarge H")
         (shrink-window-horizontally . "shrink  H")
         (shrink-window-if-larger-than-buffer . "shrink  *")
         (balance-windows . "balance *"))
        ("Mark & Kill" (set-mark-command . "mark") (kill-line . "kill line")
         (kill-region . "kill reg.") (yank . "yank")
         (exchange-point-and-mark . "swap"))
        ("IDE" (xref-find-definitions . "find definitions")
         (xref-go-back . "xref back")
         (xref-go-forward . "xref forward")
         (completion-at-point . "autocomp. <C-M-i>"))
        ("Projects" (project-switch-project . "switch")
         (project-find-file . "find file") (project-find-regexp . "search")
         (project-query-replace-regexp . "& replace")
         (project-compile . "compile"))
        ("Misc." (undo . "undo <C-z>") (undo-redo . "redo <C-S-z>")
         (isearch-forward . "search")
         (isearch-backward . "reverse search")
         (query-replace . "search & replace") (fill-paragraph . "reformat")
         (shell-command "sh cmd")
         )))

(provide 'custom-help-quick)
;;; custom-help-quick.el ends here
