;;; init.el --- Emacs init script  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(when (< emacs-major-version 30)
  (error "Minimum required Emacs version 30, current %s" emacs-major-version))
(eval-when-compile
  (require 'use-package))
;;;;; begin of configure package manager
(use-package package
  :ensure nil
  :custom
  (package-native-compile t)
  (package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                      ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                      ("melpa-stable" . "https://stable.melpa.org/packages/")
                      ("melpa" . "https://melpa.org/packages/")))
  (package-archive-priorities '(("gnu" . 10) ("nongnu" . 9)
                                ("melpa-stable" . 7) ("melpa" . 1)))
  :config
  (package-initialize)                  ; must have
  (when (eq cfg/profile 'built-in)
      (setq package-archives nil)))
(use-package use-package
  :ensure nil
  :custom
  ;; (use-package-always-defer t)
  (use-package-minimum-reported-time 0.05)
  (use-package-verbose nil))
;;;;; end of configure package manager
;;; performance
(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((mb (if (display-graphic-p) 8 1)))
              (setq gc-cons-threshold (* mb 1024 1024)))
            (message "Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f") gcs-done))
          100)


;;;
;;; lisp--cfg-setup
;;;
(defun cfg//setup--override-keybindings ()
  (keymap-global-unset "C-x C-c")       ; save-buffers-kill-terminal
  (keymap-global-set "C-x x c" 'save-buffers-kill-terminal) ; close emacs
  ;; (keymap-global-set "C-x C-c" (lambda () (interactive) (message "<C-x C-c> rebind to <C-x x c>")))
  (keymap-global-set "C-x C-c" 'kmacro-keymap) ; release <C-x C-k>
  (keymap-global-set "C-x C-k" 'kill-current-buffer) ;
  ;;; <C-;> && <C-'> && <C-,> don't working in terminal
  ;;; <C-/> readed like <C-_>
  (keymap-global-unset "C-z")         ; suspend-frame + <C-x C-z>
  (keymap-global-unset "C-?")         ; undo-redo == <C-S-z> + <C-M-_>
  (keymap-global-unset "C-/")         ; undo == <C-z> + <C-_>
  (keymap-global-unset "M-/")         ; dabbrev-expand
  (keymap-global-unset "C-x ;")       ; comment-set-column
  (keymap-global-unset "C-x C-;")     ; comment-line == <C-/>
  (keymap-global-unset "M-;")         ; comment-dwim == <M-/>
  (keymap-global-set "C-z" 'undo)     ;
  (keymap-global-set "C-S-z" 'undo-redo)  ; also <C-M-_> (in terminal)
  (keymap-global-set "C-/" 'comment-line) ; short comment line
  (keymap-global-set "M-/" 'comment-dwim) ; comment endline
  (keymap-global-set "M-;" 'dabbrev-expand) ; expand previous word "dynamically"
  
  (keymap-global-set "C-x !" 'shrink-window)
  )
(defun cfg//setup--ido-mode ()
  (ido-mode 1)
  (setf (nth 2 ido-decorations) "\n")
  (setq ido-enable-flex-matching t) ; show any name that has the chars you typed
  (setq ido-default-file-method 'selected-window) ; use current pane for newly opened file
  (setq ido-default-buffer-method 'selected-window) ; use current pane for newly switched buffer
  (setq max-mini-window-height 0.5) ; big minibuffer height, for ido to show choices vertically
  )
(defun cfg//setup--remap-major-modes ()
  (push '(ruby-mode ruby-ts-mode) major-mode-remap-alist)
  (push '(rust-mode rust-ts-mode) major-mode-remap-alist))
(cfg//setup--override-keybindings)

;;;
;;; killless-mode
;;;
(defvar backup-confirm-pred nil "Backup for `confirm-kill-emacs' variable.")
(define-minor-mode killless-mode
  "Minor mode for prevent Emacs from accidentally closing."
  :init-value nil
  :global t
  :lighter " ⛨" ; " LLL"

  (if killless-mode
      (progn
        (setq backup-confirm-pred (eval (car (get 'confirm-kill-emacs 'customized-value))))
        (setq confirm-kill-emacs (lambda (&rest args)
                                         (progn
                                           (message "Realy? Emacs is killless!")
                                           nil)))
        (message "killless ON")
        nil)
    (setq confirm-kill-emacs backup-confirm-pred)
    (setq backup-confirm-pred nil)
    (message "killless OFF")
    nil)
  )
;;; remap keybindings
(defvar-keymap cfg-hk/open-config-map
  :doc "Config"
  :prefix 'cfg-hk/open-config-map-prefix
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
(defvar-keymap cfg-hk/open-entities-map
  :doc "Open frequently used entities."
  :prefix 'cfg-hk/open-entities-map-prefix
  "r" '("open recents" . recentf-open)
  )
(defvar-keymap cfg-hk/open-shelf-map
  :doc "Open user shelf."
  :prefix 'cfg-hk/open-shelf-map-prefix
  "I" `("roam index" . (lambda ()(interactive) (find-file (cfg/org "index.org"))))
  "M" `("math" . (lambda ()(interactive) (find-file (cfg/org "math.org"))))
  "<f12>" `("Заметки" . (lambda ()(interactive) (find-file (cfg/path-s "help-daily.org")))))
(defvar-keymap cfg-hk/run-commands-map
  :doc "Run external commands."
  :prefix 'cfg-hk/run-commands-map-prefix
  ;; "a" `("emaxa" . (lambda () (interactive) (make-process :name "Emacs Air" :command "emaxa")))
  )
(defvar-keymap cfg-hk/update-ui-map
  :doc "View changes."
  :prefix 'cfg-hk/update-ui-map-prefix
  "t" '("switch theme" . cfg/switch-theme))

;; Emax major keymap
(defvar-keymap cfg-hk/major-map
  :doc "My main menu on long hand."
  :prefix 'cfg-hk/major-map-prefix
  "c" `("configure" . cfg-hk/open-config-map-prefix)
  "o" `("open" . cfg-hk/open-entities-map-prefix)
  ;; "r" `("run". cfg-hk/run-commands-map-prefix)
  "u" `("ui" . cfg-hk/update-ui-map-prefix)
  "k" '("kill less" . killless-mode)
  "C-c" '("close emacs" .  save-buffers-kill-terminal)
  "<f12>" `("shelf" . cfg-hk/open-shelf-map-prefix))
;; Emax craft keymap
(defvar-keymap cfg-hk/craft-map
  :doc "My craft menu on short hand."
  :prefix 'cfg-hk/craft-map-prefix
  "?" '("help" . (lambda () (interactive) (find-file (cfg/path-s "jotting.org"))))
  )
;; Emax minor keymap
(defvar-keymap cfg-hk/agile-map
  :doc "My minor menu on short hand."
  :prefix 'cfg-hk/agile-map-prefix
  "s" '("tab switcher" . tab-switcher)
  "t" '("treemacs" . treemacs)
  "C-e" '("w-right" . windmove-right)
  "C-f" '("w-right" . windmove-right)
  "C-a" '("w-left" . windmove-left)
  "C-b" '("w-left" . windmove-left)
  "C-p" '("w-up" . windmove-up)
  "C-n" '("w-down" . windmove-down))
;;;
;;; emax-hk-mode
;;;
(defvar-keymap emax-hk-mode-map
  :doc "Keymap for emax hotkeys mode."
  :name "name-of-hk-map"
  :prefix 'emax-hk-mode-map-prefix
  "M-j" #'execute-extended-command
  "C-j" #'Control-X-prefix
  "C-;" #'cfg-hk/agile-map-prefix
  "C-." #'cfg-hk/craft-map-prefix
  "<f12>" #'cfg-hk/major-map-prefix)
;; (defcustom emax-hk-translate-keys nil
;;   "Hide the comments too when you do an `hs-hide-all'."
;;   :type 'boolean
;;   :set )
(define-minor-mode emax-hk-mode
  "Minor mode with my emax hotkeys."
  :global t
  :lighter " ⌘" ; 🛠✧✲
  :keymap emax-hk-mode-map
  (if emax-hk-mode
      (progn
        ;; (keymap-set key-translation-map "C-j" "C-x")
        ;; (keymap-set key-translation-map "M-j" "M-x")
        (unless (display-graphic-p)
;;; <C-;> && <C-'> && <C-,> don't working in terminal
;;; <C-/> readed like <C-_>
          (keymap-set key-translation-map ";" "C-;")
          (keymap-set key-translation-map "C-x ;" ";")
          (keymap-set key-translation-map "C-_" "C-/")))
    ;; (keymap-unset key-translation-map "C-j")
    ;; (keymap-unset key-translation-map "M-j")
    (unless (display-graphic-p)
      (keymap-unset key-translation-map ";")
      (keymap-unset key-translation-map "C-x ;")
      (keymap-unset key-translation-map "C-_"))
    ))
(emax-hk-mode 1)

;;;
;;; thms--initialize
;;;
(defvar cfg/available-theme-tags '(None Craft Night Paper Retro)
  "List of available tags of themes.")
(defvar cfg/theme-profile-tags '((actual . Night)
                                 (built-in . wombat)
                                 (craft . Craft)
                                 (devel . None))
  "Table for selecting a theme by profile.")
(defvar cfg/theme-tags '((Craft . misterioso)
                         (Night . tango-dark)
                         (Paper . adwaita)
                         (Retro . deeper-blue))
  "Table for selecting a theme by tag.")
(defun cfg/switch-theme (theme &optional no-confirm no-enable)
  "Wrapper for `load-theme', load theme ARG."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar #'symbol-name
				                     (append cfg/available-theme-tags
                                             (custom-available-themes)))))
    nil nil))
  (mapcar #'disable-theme custom-enabled-themes)
  (unless (eq 'None theme)
    (load-theme (or (alist-get theme cfg/theme-tags) theme) no-confirm no-enable)))
;; override theme tags
(when (and (member cfg/profile '(actual craft)) (package-installed-p 'ef-themes))
  (setq cfg/theme-tags '((Craft . ef-maris-dark)
                         (Night . ef-night)
                         (Paper . ef-cyprus)
                         (Retro . ef-bio))))
;; load theme
(cfg/switch-theme (intern-soft (or (getenv "EMAX_THM")
                                   (alist-get cfg/profile cfg/theme-profile-tags)
                                   'None)) t)
;;; customize
(defun cfg/yard (&optional arg) (expand-file-name (or arg "") "~/sync/yard"))
(defun cfg/shelf (&optional arg) (expand-file-name (or arg "") "~/sync/shelf"))
(defun cfg/org (&optional arg) (expand-file-name (or arg "") "~/sync/shelf/org-arium"))
(custom-set-variables
 '(calendar-date-style 'european)
 '(diary-file (cfg/shelf "diary")))
(if (file-exists-p custom-file)
    (load custom-file)
  (progn
    ;;;;; visibility
    (custom-set-variables
     '(blink-cursor-blinks 3600)   ; время мигания курсора при бездействии
     '(column-number-mode t)
     '(display-line-numbers 'relative)
     '(fringe-mode 8 nil (fringe))
     '(hl-line-mode t)                 ; подсветка текущей строки
     '(inhibit-startup-screen t)
     '(menu-bar-mode nil)
     '(scroll-bar-mode nil)
     '(scroll-step 1)
     '(tab-bar-mode t)                      ; using tab bars
     '(tab-bar-show 100)                    ; hide if tabs less 100
     '(tool-bar-mode nil)
     '(visible-bell t))                ; мигание если действие не доступно
    ;;;;; saving additional information
    (custom-set-variables
     '(auto-save-default nil)
     '(auto-save-list-file-prefix (cfg/path-u "tmp-auto-save-list/.saves-"))
     '(create-lockfiles nil)
     '(make-backup-files nil)
     ;; '(project-list-file (cfg/path-u "projects"))
     '(recentf-mode t) ; use the M-x recentf-open-files command
     ;; '(recentf-save-file (cfg/path-u "recentf"))
     '(savehist-mode t)      ; history
     '(history-length 25)    ; save what you enter into minibuffer prompts
     '(history-delete-duplicates t)
     ;; '(savehist-file (cfg/path-u "history"))
     '(save-place-mode t) ; remember and restore the last cursor location of opened files
     )
    ;;;;; behaviour
    (custom-set-variables
     '(auto-revert-mode t)                  ; режим автообновления буфера
     '(auto-revert-interval 1)
     '(delete-selection-mode t)
     ;; '(global-auto-revert-mode t) ; autoreload file from disk if changed
     '(global-auto-revert-non-file-buffers t)
     '(require-final-newline t)
     '(truncate-lines t)
     '(use-short-answers t))
    ;;;;; tabs
    (custom-set-variables
     '(indent-tabs-mode nil)
     '(c-tab-always-indent t)
     '(tab-always-indent t)
     '(tab-width 4))
    ;;;;; C/C++ mode
    (c-add-style "main" '("gnu" (c-basic-offset . 4)))
    (customize-set-variable 'c-default-style
                            '((java-mode . "java") (awk-mode . "awk")
                              (other . "main")))
    ;;;;; electric-pairs
    (custom-set-variables
     '(electric-pair-mode t)
     '(electric-pair-pairs
       '((34 . 34)
         (8216 . 8217)
         (8220 . 8221)
         ;; (39 . 39) ;; ''
         (96 . 96)
         (40 . 41)
         (91 . 93)
         (123 . 125)))
     '(electric-pair-text-pairs
       '((34 . 34)
         (8216 . 8217)
         (8220 . 8221)
         ;; (39 . 39) ;; ''
         (96 . 96)
         (40 . 41)
         (91 . 93)
         (123 . 125))))
    ))
;;;
;;; lisp--emacs-customize
;;;
(when (and (display-graphic-p (selected-frame)) (eq cfg/profile 'craft))
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (setopt cursor-type 'hollow))
;;; remap tree-sitter modes
(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (css-mode . css-ts-mode)
        (js2-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (python-mode . python-ts-mode)
        (yaml-mode . yaml-ts-mode)))
;;;;; hightlight line off
(dolist (m-hook '(eshell-mode-hook
                  term-mode-hook
                  shell-mode-hook))
  (add-hook m-hook (lambda () (hl-line-mode 0))))
;;;;; display-line-numbers off
(dolist (m-hook '(eshell-mode-hook
                  eww-mode-hook
                  org-mode-hook
                  minimap-sb-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  treemacs-mode-hook
                  spacious-padding-mode-hook
                  w3m-mode-hook))
  (add-hook m-hook (lambda () (display-line-numbers-mode 0))))
;;;;; folding code
(add-hook 'prog-mode-hook #'hs-minor-mode)
;;;
;;; lisp--cfg-functions
;;;
(defun cfg/toggle-tab-bar-headers-visible (&optional visible)
  "Show/hide headers of tab bar.
   If argument VISIBLE is nil, then toggle visible of headers.
   If argument VISIBLE <= 0, then hide headers.
   If argument VISIBLE > 0, then show headers."
  (interactive)
  (let ((tbs (if visible (if ; <
                             (> visible 0) t 100)
               (if (eq tab-bar-show t) 100 t))))
    (customize-set-variable 'tab-bar-show tbs))
  (eq tab-bar-show t))
(defun emax-sh (command)
  "Run `start-process-shell-command' with COMMAND.
NAME   set to \"sh:COMMAND\"
BUFFER set to \"bf:COMMAND\"
"
  (interactive "sCommand: ")
  (let ((name (concat "process: " command))
        (buff (concat "bash: " command)))
    (start-process-shell-command name buff command)
    (display-buffer buff)))
;;;
(load (cfg/path "data/help-quick-custom"))
;;; load profile
(load (cfg/path (format "init/init-%s" cfg/profile)))

(provide 'init)
;;; init.el ends here
