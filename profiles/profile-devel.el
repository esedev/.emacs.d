;;; profile-devel.el --- Experimental Emacs config  -*- lexical-binding: t; -*-

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
;; This config used for experiments with emacs configuration

;;; Code:
;; 
;;;
;;; role--built-in--core
;;;
(use-package emacs
  :ensure nil
  :preface
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

  :init
  (defun cfg//ido-mode-setup ()
    (ido-mode 1)
    (setf (nth 2 ido-decorations) "\n")
    (setq ido-enable-flex-matching t) ; show any name that has the chars you typed
    (setq ido-default-file-method 'selected-window) ; use current pane for newly opened file
    (setq ido-default-buffer-method 'selected-window) ; use current pane for newly switched buffer
    (setq max-mini-window-height 0.5) ; big minibuffer height, for ido to show choices vertically
    )
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
;;;
;;; role--navi--vertico
;;;
(use-package cape
  :ensure t
  :pin gnu
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
	 ("C-c p t" . complete-tag)        ;; etags
	 ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
	 ("C-c p h" . cape-history)
	 ("C-c p f" . cape-file)
	 ("C-c p k" . cape-keyword)
	 ("C-c p s" . cape-elisp-symbol)
	 ("C-c p e" . cape-elisp-block)
	 ("C-c p a" . cape-abbrev)
	 ("C-c p l" . cape-line)
	 ("C-c p w" . cape-dict)
	 ("C-c p :" . cape-emoji)
	 ("C-c p \\" . cape-tex)
	 ("C-c p _" . cape-tex)
	 ("C-c p ^" . cape-tex)
	 ("C-c p &" . cape-sgml)
	 ("C-c p r" . cape-rfc1345)
     ;; ("M-;" . completion-at-point)
     )
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)
;; Example configuration for Consult
(use-package consult
  :ensure t
  :pin gnu
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
	 ("C-c M-x" . consult-mode-command)
	 ("C-c h" . consult-history)
	 ("C-c k" . consult-kmacro)
	 ("C-c m" . consult-man)
	 ("C-c i" . consult-info)
     ("C-s" . consult-line)
	 ([remap Info-search] . consult-info)
	 ;; C-x bindings in `ctl-x-map'
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ;; M-g bindings in `goto-map'
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings in `search-map'
	 ("M-s d" . consult-find)                  ;; Alternative: consult-fd
	 ("M-s c" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   ;; ERR: consult--source-bookmark is neither a Command command nor a source
   ;; consult--source-bookmark consult--source-file-register
   ;; consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)
(use-package corfu
  :ensure t
  :pin gnu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.3)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))
(use-package embark
  :ensure t
  :pin gnu
  :bind (("C-h B" . embark-bindings)
         :map cfg/kmap-major-menu
         ("a" . embark-act)
         :map cfg/kmap-minor-menu
         ("M-;" . embark-dwim))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))
(use-package embark-consult
  :ensure t
  :pin gnu
  :requires (embark consult)
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
(use-package marginalia
  :ensure t
  :pin gnu
  :config
  (marginalia-mode 1))
(use-package orderless
  :ensure t
  :pin gnu
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))
(use-package vertico
  :ensure t
  :init
  (use-package vertico-multiform
    :ensure nil
    :after vertico
    :config
    (setq vertico-multiform-categories
          '((file grid reverse)
            (consult-location buffer)
            (consult-grep buffer)
            (minor-mode reverse)
            (imenu buffer)
            (t unobtrusive)))
    :hook (after-init . vertico-multiform-mode))
  :config
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)
  ;; Show more candidates
  (setq vertico-count 12)
  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  (keymap-set vertico-map "C-l" #'vertico-exit)
  (vertico-mode 1))
(use-package vertico-posframe
  :ensure t
  :preface
  (use-package posframe :ensure t)
  :requires (vertico posframe)
  :after vertico
  :custom
  (vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
  :config
  (setq vertico-multiform-commands '(
                                     (consult-line (:not posframe))
                                     ;; (gopar/consult-line (:not posframe))
                                     ;; (consult-ag (:not posframe))
                                     (t posframe)))
  (vertico-posframe-mode 1))
(use-package hydra ; short bindings with a common prefix (GNU)
  :ensure t
  :pin gnu
  :demand t
  :config
  (defhydra hydra--toggle-mode (:hint nil)
    "
    ^
    ^Toggle^   ^Emacs^              ^Ui^               ^Editor^
    ^------^---^-----^--------------^--^---------------^------^---
    _q_ quit   _h_ highlight line   _f_ fontaine       _m_ menu
    ^^         _l_ long line wrap   _g_ golden ratio   _t_ treemacs
    _U_ ⌘      _w_ whitespaces      _o_ org modern     _T_ tab headers
    _k_ ⛨ ^^                        _p_ paddings       _W_ Win tabs
    "
    ("q" nil)
  
    ("h" global-hl-line-mode)
    ("l" visual-line-mode)
    ("w" whitespace-mode)
  
    ;; ("f" fontaine-set-preset)
    ("f" fontaine-mode)
    ("g" golden-ratio-mode)
    ("o" org-modern-mode)
    ("p" spacious-padding-mode)
  
    ("m" menu-bar-mode)
    ("t" treemacs)
    ("T" u--toggle-tab-bar-headers-visible)
    ("W" tab-line-mode)
  
    ("U" user-trmap-mode)
    ("k" killless-mode))
  
  (defhydra hydra--play-games (:hint nil :color blue)
    "
    ^
      ^Games
      ^-------------
      _!_ happy birthday
      _D_ dunnet
      _g_ gomoku
      _l_ life
      _m_ mines
      _M_ mpuz
      _s_ snake
      _t_ solitaire
      _z_ zone
      _d_ doctor
  
      _q_ quit
      ^-------------
      "
    ("!" animate-birthday-presint)
    ("D" dunnet)
    ("g" gomoku)
    ("l" life)
    ("m" mines)
    ("M" mpuz)
    ("s" snake)
    ("t" solitaire)
    ("z" zone)
    ("d" doctor)
    ("q" nil :color pink)
  )
  (defhydra hydra-helper--buf-move()
    "move buffer"
    ("s" buf-move-left "left")
    ("e" buf-move-up "up")
    ("d" buf-move-down "down")
    ("f" buf-move-right "right")
    ("SPC" nil "finished", :exit t))
  
  (keymap-set cfg/kmap-update-ui "C-t" '("text scale" . hydra--text-scale/body))
  (keymap-set cfg/kmap-update-ui "w" '("window move" . hydra--window-move/body))
  (keymap-set cfg/kmap-major-menu "g" '("games" . hydra--play-games/body))
  (keymap-set cfg/kmap-major-menu "t" '("toggle" . hydra--toggle-mode/body))
  
  (keymap-global-set "M-O" 'hydra--window-move/body); window move
)
(use-package ace-window
  :ensure t
  :pin gnu
  :config
  (keymap-global-set "M-o" 'ace-window)
  (setq aw-dispatch-always t)
  )
(use-package delight :ensure t :pin gnu)
(use-package golden-ratio ; auto resize window by golden ratio
  :ensure t
  :pin nongnu
  ;; :hook (after-init . golden-ratio-mode)
  :custom
  (golden-ratio-exclude-modes '(occur-mode))
  :config
  (keymap-global-set "C-x #" 'golden-ratio))
(use-package move-text                  ; melpa-stable
  :ensure t                             ; Using the prefix (C-u *number* or META *number*)
  :demand t                             ; you can predefine how many lines move-text will travel.
  :config (move-text-default-bindings))
(use-package rainbow-mode ; colorize color names in buffers
  :ensure t
  :pin gnu
  ;; :delight
  :hook (prog-mode . rainbow-mode))
(use-package rainbow-delimiters                ; highlight brackets according
  :ensure t
  :pin nongnu
  :hook (prog-mode . rainbow-delimiters-mode)) ; to their depth


;; ;;;
;; ;;; role--experimental
;; ;;;
;; 
;; ;;;
;; ;;; role--yard-of-test
;; ;;;
;; (use-package flycheck
;;   :ensure t
;;   :hook (after-init . global-flycheck-mode)
;;   :config
;;   (keymap-set cfg/kmap-craft-menu "f" flycheck-command-map)
;;   (which-key-add-key-based-replacements "C-c !" "Flycheck")
;;   (which-key-add-key-based-replacements "C-. f" "Flycheck"))
;; (use-package lsp-mode
;;   :ensure t
;;   :init
;;   (defun cfg/lsp-mode-setup ()
;;       (keymap-set cfg/kmap-craft-menu "m" '("imenu" . lsp-ui-imenu)))
;;   :hook (lsp-mode . cfg/lsp-mode-setup)
;;   :custom (lsp-keymap-prefix "C-. l")
;;   )
;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
;; (use-package lsp-treemacs
;;   :ensure t
;;   :requires (lsp-mode treemacs)
;;   :after lsp-mode)
;; (use-package lsp-ui
;;   :ensure t
;;   :requires lsp-mode)
;; (use-package rustic
;;   :ensure t
;;   :delight "R✧stic!"
;;   :custom
;;   (rustic-cargo-use-last-stored-arguments t)
;;   (rustic-format-on-save t)
;;   :config
;;   (which-key-add-key-based-replacements "C-c C-c" "Rustic"))

(provide 'profile-devel)
;;; profile-devel.el ends here
