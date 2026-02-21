;;; init.el --- Emacs init script  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(when (< emacs-major-version 30)
  (error "Minimum required Emacs version 30, current %s" emacs-major-version))

;;; begin of configure package manager
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities '(("gnu" . 10)
                                   ("nongnu" . 8)
                                   ("melpa-stable" . 5)
                                   ("melpa" . 1))
      )
(eval-when-compile
  (require 'use-package))
(when (eq 'built-in cfg/profile)
  (setq package-archives nil))
(use-package use-package
  ;; :custom
  ;; (use-package-always-defer t)
  ;; (use-package-verbose t)
  ;; (use-package-minimum-reported-time 0.005)
  )
;;; end of configure package manager

;;;
;;; role--initialize
;;;
(defmacro use-feature (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))
(defmacro use-feature-site (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :if (require ',name nil 'noerror)
     :ensure nil
     ,@args))

(use-package emacs
  :init
  (defun cfg/yard (arg) (expand-file-name (or arg "") "~/sync/yard"))
  (defun cfg/shelf (arg) (expand-file-name (or arg "") "~/sync/shelf"))
  (defun cfg/display-startup-time ()
    (message "Emacs loaded in %s seconds with %d garbage collections."
             (emacs-init-time "%.2f") gcs-done))
  ;;;
  ;;; code--my-actions
  ;;;
  ;; toggle tab-bar headers visible
  (defun u--toggle-tab-bar-headers-visible (&optional visible)
    "Show/hide headers of tab bar.
     If argument VISIBLE is nil, then toggle visible of headers.
     If argument VISIBLE <= 0, then hide headers.
     If argument VISIBLE > 0, then show headers."
    (interactive)
    (let ((tbs (if visible (if ; <
                                            (> visible 0) t 100)
                              (if (eq tab-bar-show t) 100 t))))
      (customize-set-variable 'tab-bar-show tbs))
    (eq tab-bar-show t)
    )
  ;; show daily help
  (defun u--help-daily-toggle ()
    "The daily help text, UNIMPLEMENTED."
    (interactive)
    (message "User daily help text")
    (display-buffer "Daily help"))
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
   '(history-length 25) ; save what you enter into minibuffer prompts
   '(history-delete-duplicates t)
   '(require-final-newline t)
   '(save-place-mode t) ; remember and restore the last cursor location of opened files
   '(truncate-lines t)
   '(calendar-date-style 'european)
   '(diary-file (cfg/shelf "diary"))
   )
  ;;;;; C/C++ mode
  (c-add-style "main"
               '("gnu"
                 (c-basic-offset . 4)
                 ; other
                 ;; (c-offsets-alist
                  ;; ()
                  ;; )
                 ))
  (customize-set-variable 'c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "main")))
  ;;;;; hightlight line off
  (dolist (mode '(eshell-mode-hook
                    term-mode-hook
                    shell-mode-hook))
      (add-hook mode (lambda () (hl-line-mode 0))))
  ;;;;; line-numbers off
  (dolist (mode '(eshell-mode-hook
                  minimap-sb-mode-hook
                  org-mode-hook
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
  (setq major-mode-remap-alist
   '((yaml-mode . yaml-ts-mode)
     (bash-mode . bash-ts-mode)
     (js2-mode . js-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (json-mode . json-ts-mode)
     (css-mode . css-ts-mode)
     (python-mode . python-ts-mode)
     (ruby-mode . ruby-ts-mode)
     (rust-mode . rust-ts-mode)))

  :config
  (when (display-graphic-p (selected-frame))
    (setopt cursor-type 'hollow))
  (add-hook 'emacs-startup-hook #'cfg/display-startup-time)
  (user-trmap-mode 1))
;;;
;;; thms--initialize
;;;
(use-package emacs
  :init
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
    (cfg/set-title-tail (symbol-name theme))
    (unless (eq 'None theme)
      (load-theme (or (alist-get theme cfg/theme-tags) theme) no-confirm no-enable)))
  :config
  (unless (member cfg/profile '(built-in devel))
    (use-feature-site ef-themes
      :config
      (setq cfg/theme-tags '((Craft . ef-maris-dark)
                             (Night . ef-night)
                             (Paper . ef-cyprus)
                             (Retro . ef-bio)))))

  (cfg/switch-theme (intern-soft (or (getenv "EMAX_THM")
                                     (alist-get cfg/profile cfg/theme-profile-tags)
                                     'None)) t))

(let ((fname (cfg/path "data/help-quick-custom.el")))
  (when (file-exists-p fname) (load fname)))

;;; load profile
(load (cfg/path (format "profiles/profile-%s" cfg/profile)))

;;; deferred activations
(which-key-mode)

(provide 'init)
;;; init.el ends here
