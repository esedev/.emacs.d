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
;;;
;;; role--built-in--core
;;;
(use-package emacs
  :ensure nil
  :init
  (defvar-keymap cfg/kmap-open-config :full t
    :doc "Config"
    "1" '("Config" . (lambda () (interactive) (find-file (cfg/path "Config.org"))))
    ;; "1" '("Config" . (i-event (find-file (cfg/path "Config.org"))))
    "2" '("Cfg directory" . (lambda () (interactive) (project-switch-project (cfg/path ""))))
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
  ;; Craft IDE menu
  (defvar-keymap cfg/kmap-craft-menu
    :doc "My craft menu on short hand."
    "?" '("help" . (lambda () (interactive) (find-file (cfg/path-s "jotting.org"))))
    )
  ;; Minor user menu
  (defvar-keymap cfg/kmap-minor-menu
    :doc "My minor menu on short hand."
    "s" '("tab switcher" . tab-switcher)
    "t" '("treemacs" . treemacs)
    "C-e" '("w-right" . windmove-right)
    "C-f" '("w-right" . windmove-right)
    "C-a" '("w-left" . windmove-left)
    "C-b" '("w-left" . windmove-left)
    "C-p" '("w-up" . windmove-up)
    "C-n" '("w-down" . windmove-down))
  ;; Major user menu
  (defvar-keymap cfg/kmap-major-menu
    :doc "My main menu on long hand."
    "c" `("configure" . ,cfg/kmap-open-config)
    "o" `("open" . ,cfg/kmap-open-entities)
    ;; "r" `("run". ,cfg/kmap-run-commands)
    "u" `("ui" . ,cfg/kmap-update-ui)
    "k" '("kill less" . killless-mode)
    "C-c" '("close emacs" .  save-buffers-kill-terminal)
    "<f12>" `("shelf" . ,cfg/kmap-user-shelf))
  ;;
  (keymap-set global-map "C-." cfg/kmap-craft-menu)
  (keymap-set global-map "C-;" cfg/kmap-minor-menu)
  (keymap-set global-map "<f12>" cfg/kmap-major-menu)
  )

;; (use-package desktop
;;   :ensure nil
;;   :custom
;;   (desktop-dirname (cfg/path-u "desktop") "Каталог для хранения файла .desktop.")
;;   ;; (desktop-load-locked-desktop t "Загрузка файла .desktop даже если он заблокирован.")
;;   (desktop-restore-frames t "Восстанавливать фреймы.")
;;   (desktop-save t "Сохранять список открытых буферов, файлов и т. д. без лишних вопросов.")
;;   ;; :hook (after-init . desktop-read)
;;   :config
;;   ;; (desktop-save-mode t)
;;   ;; (add-to-list 'delete-frame-functions 'desktop-save)
;;   (add-to-list 'desktop-modes-not-to-save 'dired-mode))
(use-package dired
  :ensure nil
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
  (auto-revert-verbose nil)
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . auto-revert-mode)
  ;; (dired-mode . dired-omit-mode) ; <C-x M-o> - toggle dired-omit-mode
  :config
  (put 'dired-find-alternate-file 'disabled nil))
(use-package eww
  :ensure nil
  :defer t
  :preface
  (defun eww-render-buffer ()
    "Render the current buffer in EWW."
    (interactive)
    (let* ((html (buffer-substring-no-properties (point-min) (point-max)))
           (source (buffer-name))
           (buf (generate-new-buffer (concat "eww: " source))))
      (with-current-buffer buf
        (insert html)
        (goto-char (point-min))
        (eww-display-html 'utf-8 source nil (point-min) buf))
      (switch-to-buffer buf)))
  :config
  (setq eww-search-prefix "https://lite.duckduckgo.com/lite/?q=")
  (keymap-set cfg/kmap-user-shelf "<f2>"
              '("Руководство по GNU Emacs" .
                (lambda ()(interactive)(eww "https://alexott.net/ru/emacs/emacs-manual/"))))
  (keymap-set cfg/kmap-user-shelf "<f3>"
              `("Поиск в сети" .
                (lambda ()(interactive)(eww "https://lite.duckduckgo.com/lite"))))
  (keymap-set cfg/kmap-user-shelf "<f4>"
              '("eww: текущий буфер" .
                eww-render-buffer)))
(use-package org
  :ensure nil
  :pin manual
  :defer t
  :hook  (org-mode . u-hook--org-mode-setup)
  :init
  (defun u-hook--org-mode-setup ()
    "Minor modes tunning."
    (org-indent-mode 1)
    (variable-pitch-mode 0)
    (visual-line-mode 1))
  (defun cfg/org (arg)
    "Return path to ARG file."
    (expand-file-name arg (cfg/shelf "org-arium")))
  (keymap-set cfg/kmap-open-entities "a" '("agenda" . org-agenda))
  (keymap-set cfg/kmap-open-entities "s" '("schedule" . org-agenda-list))
  :bind-keymap
  ("C-c o" . org-mode-map)
  :custom
  (org-directory (cfg/org ""))
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
  (org-confirm-babel-evaluate nil)
  :config
  (add-to-list 'org-src-lang-modes
               (cons "D" 'd)
               (cons "conf-unix" 'conf-unix))
  ;; (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (shell . t)
     (ruby . t)
     (python . t)))
  (which-key-add-key-based-replacements "C-c o" "Org")
  (which-key-add-major-mode-key-based-replacements 'org-mode
    "C-c \"" "Org plot" ; \"
    "C-c C-v" "Org babel"
    "C-c C-x" "Org X"))
(use-package ruby-ts-mode
  :ensure nil
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
(use-package term
  :ensure nil
  :commands term
  :config
  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  ;;(setq explicit-zsh-args '())         ; Use 'explicit-<shell>-args for shell-specific args
  ;; (setq explicit-shell-file-name "bash") ; Change this to zsh, etc
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>λ] *"))
(use-package which-key
  :ensure nil
  :delight
  :hook (emacs-startup . which-key-mode)
  :config
  (setq which-key-idle-delay 0.4)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-min-display-lines 4)

  ;; (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
  ;; (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
  ;; (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
  ;; (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil)))

  (which-key-add-key-based-replacements "C-c @" "ShowHide")
  (which-key-add-key-based-replacements "C-c e" "IDE")

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
  :ensure nil
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
(defun cfg//ido-mode-setup ()
  (ido-mode 1)
  (setf (nth 2 ido-decorations) "\n")
  (setq ido-enable-flex-matching t) ; show any name that has the chars you typed
  (setq ido-default-file-method 'selected-window) ; use current pane for newly opened file
  (setq ido-default-buffer-method 'selected-window) ; use current pane for newly switched buffer
  (setq max-mini-window-height 0.5) ; big minibuffer height, for ido to show choices vertically
  )
(defun cfg//remap-major-modes ()
  (push '(ruby-mode ruby-ts-mode) major-mode-remap-alist)
  (push '(rust-mode rust-ts-mode) major-mode-remap-alist))
(use-package flymake
  :ensure nil
  :custom
  (flymake-no-changes-timeout 1.2))
  ;; :hook((emacs-lisp-mode . flymake-mode)))
(use-package eglot
  :ensure nil
  :defer t
  :init
  (defun cfg//manually-activate-flymake ()
    (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
    (flymake-mode 1))
  (defvar-keymap cfg/kmap-eglot
    :doc "lsp eglot."
    :name "Eglot menu"
    "F" 'eglot-format
    "f" 'eglot-code-action-quickfix
    "r" 'eglot-rename
    "<f12>" 'eglot-ensure)
  (keymap-set cfg/kmap-craft-menu "e" cfg/kmap-eglot)
  (which-key-add-key-based-replacements "C-. e" "Eglot")
  ;; :hook (eglot--managed-mode . cfg//manually-activate-flymake)
  :custom
  (eglot-autoshutdown t)
  ;; :config
  ;; (add-to-list 'eglot-stay-out-of 'flymake)
  )
(use-package rust-ts-mode
  :ensure nil
  :delight "R✧st"
  :mode "\\.rs\\'"
  :hook
  (rust-ts-mode . eglot-ensure))
(cfg//remap-major-modes)

(cfg//ido-mode-setup)

(provide 'profile-built-in)
;;; profile-built-in.el ends here
