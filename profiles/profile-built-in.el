;;; profile-built-in.el --- Minimalistic Emacs config  -*- lexical-binding: t; -*-

;; WARNING: this script generated form Config.org file.
;; Copyright (c) 2024-2026 Sergey Egorov
;; Author: Segey Egorov <esedev@gmail.com>
;; Keywords: config emacs
;; Version: 0.3.0
;; Package-Requires: ((emacs "30"))
;; Created: May 2024
;; URL:
;; Repository:

;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This config use only built-in packages

;;; Code:
;; 
;;;
;;; role--built-in--core
;;;
(use-package dired
  :delight "Dir"
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  ;; (dired-omit-files "^\\..*$") ; Omit the dotfiles
  ;; (dired-omit-files "^\\.$" )
  (dired-isearch-filenames 'dwim)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-listing-switches "-aluFh --group-directories-first -I .")
  (dired-dwim-target t)
  (delete-by-moving-to-trash t)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . auto-revert-mode)
  ;; (dired-mode . dired-omit-mode) ; <C-x M-o> - toggle dired-omit-mode
  :config
  (put 'dired-find-alternate-file 'disabled nil))
(use-package emacs
  :init
  (defvar-keymap cfg/kmap-open-config :full t
    :doc "Config"
    "1" '("Config" . (lambda () (interactive) (find-file (cfg/path "Config.org"))))
    ;; "1" '("Config" . (i-event (find-file (cfg/path "Config.org"))))
    "2" '("Cfg directory" . (lambda () (interactive) (find-file (cfg/path ""))))
    "3" '("Emacs early-init.el" . (lambda () (interactive) (find-file early-init-file)))
    "4" '("Emacs init.el" . (lambda () (interactive) (find-file user-init-file)))
    "5" '("Emacs custom-file" . (lambda () (interactive)
                                  (if (file-exists-p custom-file)
                                     (find-file custom-file)
                                    (message (format "file '%s' does not exist" custom-file)))))
    "6" '("Emacs user dir" . (lambda () (interactive) (find-file user-emacs-directory)))
    "Y" '("Yard directory" . (lambda () (interactive) (find-file (cfg/yard))))
    "y" '("Yard config" . (lambda () (interactive) (find-file (cfg/yard "yard.org"))))
    )
  (defvar-keymap cfg/kmap-open-entities
    :doc "Open frequently used entities."
    "r" '("open recents" . recentf-open)
    )
  (defvar-keymap cfg/kmap-run-commands
    :doc "Run external commands."
    "a" `("emaxa" . (lambda () (interactive) (make-process :name "Emacs Air" :command "emaxa")))
    )
  (defvar-keymap cfg/kmap-user-shelf
    :doc "Open user shelf."
    "I" `("roam index" . (lambda ()(interactive) (find-file (cfg/org "index.org"))))
    "M" `("math" . (lambda ()(interactive) (find-file (cfg/org "math.org"))))
    "<f12>" `("Заметки" . (lambda ()(interactive) (find-file (cfg/path-s "help-daily.org")))))
  (defvar-keymap cfg/kmap-update-ui
    :doc "View changes."
    "T" '("load theme" . cfg/load-theme)
    "t" '("switch theme" . cfg/switch-theme))
  (defvar-keymap cfg/kmap-minor-menu
    :doc "My minor menu on short hand."
    "t" '("tab switch" . tab-switcher)
    "C-e" '("w-right" . windmove-right)
    "C-f" '("w-right" . windmove-right)
    "C-a" '("w-left" . windmove-left)
    "C-b" '("w-left" . windmove-left)
    "C-p" '("w-up" . windmove-up)
    "C-n" '("w-down" . windmove-down))
  (defvar-keymap cfg/kmap-major-menu
    :doc "My main menu on long hand."
    "c" `("configure" . ,cfg/kmap-open-config)
    "o" `("open" . ,cfg/kmap-open-entities)
    ;; "r" `("run". ,cfg/kmap-run-commands)
    "u" `("ui" . ,cfg/kmap-update-ui)
    "k" '("kill less" . killless-mode)
    "C-c" '("close emacs" .  save-buffers-kill-terminal)
    "<f12>" `("shelf" . ,cfg/kmap-user-shelf))
  (keymap-set global-map "C-;" cfg/kmap-minor-menu)
  (keymap-set global-map "<f12>" cfg/kmap-major-menu)
  (defun cfg/display-startup-time ()
    (message "Emacs loaded in %s seconds with %d garbage collections."
             (emacs-init-time "%.2f") gcs-done))
  (add-hook 'emacs-startup-hook #'cfg/display-startup-time)
  )

(use-package desktop
  :custom
  (desktop-dirname user-emacs-directory "Каталог для хранения файла .desktop.")
  ;; (desktop-load-locked-desktop t "Загрузка файла .desktop даже если он заблокирован.")
  (desktop-restore-frames t "Восстанавливать фреймы.")
  (desktop-save t "Сохранять список открытых буферов, файлов и т. д. без лишних вопросов.")
  ;; :hook (after-init . desktop-read)
  :config
  ;; (desktop-save-mode t)
  ;; (add-to-list 'delete-frame-functions 'desktop-save)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode))
(use-package eww
  :init
  (defun eww-render-buffer ()
    "Render the current buffer in EWW."
    (interactive)
    (let* ((html (buffer-substring-no-properties (point-min) (point-max)))
           (source (buffer-name))
           (buf (generate-new-buffer (concat "eww: " source))))
      (with-current-buffer buf
        (insert html)
        (goto-char (point-min))
        (eww-display-html 'utf-8 source nil nil buf))
      (switch-to-buffer buf)))
  :config
  (keymap-set cfg/kmap-user-shelf "<f1>"
              '("Руководство по GNU Emacs" .
                (lambda ()(interactive)(eww "https://alexott.net/ru/emacs/emacs-manual/"))))
  (keymap-set cfg/kmap-user-shelf "<f3>"
              `("Поиск в сети" .
                (lambda ()(interactive)(eww "https://lite.duckduckgo.com"))))
  )
(use-package flymake
  :init
  ;; Manually re-enable Eglot's Flymake backend
  (defun manually-activate-flymake ()
    (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
    (flymake-mode 1))
  :custom
  (flymake-no-changes-timeout 1.2)
  :hook((emacs-lisp-mode . flymake-mode)
        (ruby-ts-mode . flymake-mode)))
(use-package eglot
  :init
  (defvar-keymap cfg/kmap-eglot
    :doc "lsp eglot."
    :name "Eglot menu"
    "F" '("format" . eglot-format)
    "f" '("quic fix" . eglot-code-action-quickfix)
    "r" '("rename" . eglot-rename)
    "<f12>" '("run eglot" . eglot-ensure))
  (keymap-set cfg/kmap-minor-menu "e" cfg/kmap-eglot)
  :bind (("C-." . eglot-code-action-quickfix))
  :custom
  (eglot-autoshutdown t))
(use-package org
  :init
  (defun u-hook--org-mode-setup ()
    "Minor modes tunning."
    (org-indent-mode 1)
    (variable-pitch-mode 0)
    (visual-line-mode 1))
  (defun cfg/org (arg)
    "Return path to ARG file."
    (expand-file-name arg org-directory)
    )
  (keymap-set cfg/kmap-open-entities "a" '("agenda" . org-agenda))
  (keymap-set cfg/kmap-open-entities "s" '("schedule" . org-agenda-list))
  :bind-keymap
  ("C-c o" . org-mode-map)
  :custom
  (org-startup-folded 'overview)
  (org-edit-src-content-indentation 0)
  (org-agenda-current-time-string "← now ———————————————————— ☢")
  (org-ellipsis "..⯆") ; ⮋⮧⮷⯆⇲↴▾
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t) ; show inline images
  (org-return-follows-link t) ; follow by links of RET
  (org-todo-keywords
   '((sequence "TODO(t)" "WORK(g)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")))
  ;; agenda
  (org-agenda-files (list (concat org-directory "/agenda")))
  :config
  (add-to-list 'org-src-lang-modes
               (cons "D" 'd)
               (cons "conf-unix" 'conf-unix))
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (shell . t)
     (ruby . t)
     (python . t)))
  :hook
  (org-mode . u-hook--org-mode-setup))
(use-feature ruby-ts-mode
  :delight "R✧by"
  :mode "\\.rb\\'"
  :mode "config.ru\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :mode "Vagrantfile\\'"
  :interpreter "ruby"
  :bind (:map ruby-ts-mode-map
              ("C-c r b" . 'treesit-beginning-of-defun)
              ("C-c r e" . 'treesit-end-of-defun))
  :hook (ruby-base-mode . subword-mode)
  :custom (ruby-indent-level 2)
          (ruby-indent-tabs-mode nil))
(use-package rust-ts-mode
  :delight "R✧st"
  :mode "\\.rs\\'"
  :hook
  (rust-ts-mode . eglot-ensure)
  )
(use-package term
  :commands term
  :config
  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  ;;(setq explicit-zsh-args '())         ; Use 'explicit-<shell>-args for shell-specific args
  ;; (setq explicit-shell-file-name "bash") ; Change this to zsh, etc
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>λ] *"))
(use-package which-key
  :delight
  :config
  (setq which-key-idle-delay 0.6)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-min-display-lines 4)

  ;; (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
  ;; (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
  ;; (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
  ;; (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil)))

  (which-key-add-key-based-replacements "C-; e" "Eglot")
  (which-key-add-key-based-replacements "C-c &" "Yas")
  (which-key-add-key-based-replacements "C-c o" "Org")
  (which-key-add-key-based-replacements "C-c e" "IDE")
  (which-key-add-major-mode-key-based-replacements 'org-mode
    "C-c \"" "Org plot" ; \"
    "C-c C-v" "Org babel"
    "C-c C-x" "Org X")
  (which-key-add-major-mode-key-based-replacements 'rust-mode
    "C-c C-c" "Rust")
  (which-key-add-key-based-replacements "C-c p" "Cape")
  (which-key-add-key-based-replacements "C-x RET" "coding system")
  (which-key-add-key-based-replacements "C-x a" "abbrev")
  (which-key-add-key-based-replacements "C-x n" "narrow")
  (which-key-add-key-based-replacements "C-x p" "project")
  (which-key-add-key-based-replacements "C-x r" "register")
  (which-key-add-key-based-replacements "C-x t" "tab")
  (which-key-add-key-based-replacements "C-x v" "version ctrl")
  (which-key-add-key-based-replacements "C-x w" "window")
  (which-key-add-key-based-replacements "C-x x" "misc")
  (which-key-add-key-based-replacements "C-x 8" '("unicode" . "Unicode keys"))
  (which-key-add-key-based-replacements "C-x 8 e" "emoji"))
(use-package whitespace
  :config
  (setq whitespace-display-mappings
        '((space-mark 32
                      [183]
                      [46])
          (space-mark 160
                      [164]
                      [95])
          (newline-mark 10
                        [172 10])
          ;; (newline-mark 10 [36 10]))
          (tab-mark 9
                    [187 9]
                    [92 9])))
  ;; delete trailing whitespaces when save buffer
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))
(use-feature-site auto-complete)
(use-feature-site company-mode
  :delight "CompAny"
  :hook
  (rust-ts-mode . company-mode)
  )
(use-feature-site dockerfile-mode
  :init
  (defun cfg/test ()
    "NO DOC"
    (interactive)
    (message "TEST IS SUCCESSFULLY!!!")))

(use-feature-site magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(use-feature-site yaml-mode)


(use-package rust-ts-mode
  :delight "R✧st"
  :mode "\\.rs\\'"
  :hook
  (rust-ts-mode . eglot-ensure)
  )
(provide 'profile-built-in)
;;; profile-built-in.el ends here
