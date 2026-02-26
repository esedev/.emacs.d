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

;;;
;;; role--built-in--init
;;;
(use-package emacs
  :init
  (defun cfg/yard (&optional arg) (expand-file-name (or arg "") "~/sync/yard"))
  (defun cfg/shelf (&optional arg) (expand-file-name (or arg "") "~/sync/shelf"))
  (defun cfg/display-startup-time ()
    (message "Emacs loaded in %s seconds with %d garbage collections."
             (emacs-init-time "%.2f") gcs-done))

  ;;;
  ;;; bind--override-standard
  ;;;
  (defun cfg/bind--override-standard ()
    "Rebind standard bindings"
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
  (cfg/bind--override-standard)

  ;;;
  ;;; code--emacs-user-functions
  ;;;
  ;; toggle tab-bar headers visible
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
  (define-minor-mode user-trmap-mode
    "Minor mode which change `key-translation-map'."
    :init-value nil
    :global t
    :lighter " ⌘" ; ✧✲
    (if user-trmap-mode
        (progn
          (keymap-set key-translation-map "C-j" "C-x")
          (keymap-set key-translation-map "M-j" "M-x")
          (unless (display-graphic-p)
  ;;; <C-;> && <C-'> && <C-,> don't working in terminal
  ;;; <C-/> readed like <C-_>
            (keymap-set key-translation-map ";" "C-;")
            (keymap-set key-translation-map "C-x ;" ";")
            (keymap-set key-translation-map "C-_" "C-/"))
          (message "User key translation enabled"))
      (progn
        (keymap-unset key-translation-map "C-j")
        (keymap-unset key-translation-map "M-j")
        (unless (display-graphic-p)
          (keymap-unset key-translation-map ";")
          (keymap-unset key-translation-map "C-x ;")
          (keymap-unset key-translation-map "C-_"))
        (message "User key translation disabled"))
      ) user-trmap-mode)

  ;;;
  ;;; cstm--emacs-hards
  ;;;
  (custom-set-variables
   '(c-tab-always-indent t)
   '(indent-tabs-mode nil)
   '(tab-always-indent t)
   '(tab-width 4)
   '(delete-selection-mode t)
   '(global-auto-revert-mode t) ; autoreload file from disk if changed
   '(global-auto-revert-non-file-buffers t)
   '(auto-revert-interval 1)
   '(history-length 25) ; save what you enter into minibuffer prompts
   '(history-delete-duplicates t)
   '(require-final-newline t)
   '(save-place-mode t) ; remember and restore the last cursor location of opened files
   '(truncate-lines t)
   '(calendar-date-style 'european)
   '(diary-file (cfg/shelf "diary"))
   )
  ;;;;; C/C++ mode
  (c-add-style "main" '("gnu" (c-basic-offset . 4)))
  (customize-set-variable 'c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "main")))
  ;;;;; hightlight line off
  (dolist (mode '(eshell-mode-hook
                    term-mode-hook
                    shell-mode-hook))
      (add-hook mode (lambda () (hl-line-mode 0))))
  ;;;;; display-line-numbers off
  (dolist (mode '(eshell-mode-hook
                  eww-mode-hook
                  org-mode-hook
                  minimap-sb-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  treemacs-mode-hook
                  spacious-padding-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))
  
  ;;; electric-pairs
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

  (user-trmap-mode 1)
  :hook ((prog-mode . hs-minor-mode)     ; folding code
         (emacs-startup . cfg/display-startup-time))
  :config
  (setq major-mode-remap-alist
        '((bash-mode . bash-ts-mode)
          (css-mode . css-ts-mode)
          (js2-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (python-mode . python-ts-mode)
          (yaml-mode . yaml-ts-mode)))

  (when (display-graphic-p (selected-frame))
    (setopt cursor-type 'hollow))
  )
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

;;; load profile
(load (cfg/path (format "profiles/profile-%s" cfg/profile)))

(load (cfg/path "data/help-quick-custom"))

(provide 'init)
;;; init.el ends here
