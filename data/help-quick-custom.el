;;; help-quick-custom.el --- My Emacs config  -*- lexical-binding: t; -*-

;; Value should be a list of elements, each element should of the form
;;   (GROUP-NAME (COMMAND . DESCRIPTION) (COMMAND . DESCRIPTION)...)
(defvar help-quick-1 '("None" (save-buffers-kill-terminal . "exit")) "Set 1 for help quick.")
(defvar help-quick-2 '("None" (save-buffers-kill-terminal . "exit")) "Set 2 for help quick.")
(defvar help-quick-3 '("None" (save-buffers-kill-terminal . "exit")) "Set 3 for help quick.")
;;; help-quick-1
(setq help-quick-1
      '(
        ("Emacs Now"
         (consult-imenu . "imenu") (imenu . "imenu")
         (bookmark-set . "bookmark")
         (jump-to-register . "bookmark jump")
         (where-is . "where is"))
        ("IDE"
         (xref-find-definitions . "find definitions")
         (xref-go-back . "xref back")
         (xref-go-forward . "xref forward")
         (consult-flymake . "diagnostic")
         (completion-at-point . "autocomp. <C-M-i>"))
        ("Mark & Kill"
         (set-mark-command . "mark")
         (cycle-spacing . "1 space")
         (kill-line . "kill line")
         (kill-region . "kill reg.")
         (yank . "yank")
         (exchange-point-and-mark . "swap")
         (indent-region . "indent reg."))
        ("Window"
         (toggle-window-dedicated . "dedicate")
         (enlarge-window . "↑ enlarge")
         (shrink-window . "↓ shrink")
         (enlarge-window-horizontally . "←expand→")
         (shrink-window-horizontally .  "→shrink←")
         (shrink-window-if-larger-than-buffer . "by buff -")
         (balance-windows . "balance +")
         (golden-ratio . "golden  Φ")
         (other-tab-prefix . "other tab")
         (tab-bar-history-back . "tab undo")
         (winner-undo . "win undo"))
        ("Misc."
         (undo . "undo <C-z>") (undo-redo . "redo <C-S-z>")
         (isearch-forward . "search")
         (isearch-backward . "reverse search")
         (query-replace . "search & replace")
         (fill-paragraph . "reformat")
         (shell-command "sh cmd")
         (read-only-mode . "readonly")
         (save-buffers-kill-terminal . "exit"))
        ))
;;; help-quick-2
;;; help-quick-3

(provide 'help-quick-custom)
;;; help-quick-custom.el ends here
