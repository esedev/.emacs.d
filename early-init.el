;;; early-init.el --- Emacs init script  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;;; processing extended command line arguments with '--u-' prefix
(when t
  (require 'cl-seq)
  (let ((u-args (cl-remove-if-not
                 (lambda (e) (string-prefix-p "--u-" e))
                 command-line-args)))
    (dolist (a u-args)
      (when-let ((arg-lst (split-string a "="))
                 (opt (substring (car arg-lst) 4 nil))
                 (var (concat "EMAX_" (upcase opt)))
                 (val (car (cdr arg-lst))))
        (setenv var val)))
    (cl-delete-if (lambda (e) (string-prefix-p "--u-" e)) command-line-args)))

;;; initialize config variables
(defvar cfg/profile (intern (or (getenv "EMAX_PRF") "actual"))
  "User Emacs profile identity.")
(defvar cfg/job (getenv "EMAX_JOB") "Job identity, type string.")
(defvar cfg/title (format "%s%s☀%s" cfg/profile
                          (if cfg/job (concat "-" cfg/job) "")
                          system-name) "Frame title of Emacs.")
(defvar cfg/title-tail "" "Additional title info, type string.")

(defun cfg/path (arg)
  "Return full path file ARG in `user-emacs-directory'."
  (expand-file-name arg user-emacs-directory))
(defun cfg/path-s (arg)
  (expand-file-name (format "save/%s" arg) user-emacs-directory))
(defun cfg/path-t (arg)
  (expand-file-name (format "target/%s" arg) user-emacs-directory))

(defun cfg/set-title-tail (arg)
  "Set value `cfg/title-tail' as ARG."
       (concat "  [" arg "]"))

;;; micro tuning of Emacs
;; (setq frame-title-format '( "GNU/Emacs " cfg/title))
(setq frame-title-format
      '("GNU/Emacs"
        (:eval (if tab-bar-mode
                (concat " ___/ " (cdr (assq 'name (tab-bar--current-tab))) " \\___ ") ""))
         "<"cfg/title ">" cfg/title-tail))
;;; environment
(setq package-user-dir
      (cond ((eq cfg/profile 'built-in) "/tmp/emacs-elpa")
            (t (cfg/path-t "elpa"))))
(when (boundp 'native-comp-eln-load-path) ; set eln-cache dir
  (startup-redirect-eln-cache (cfg/path-t "eln-cache")))
; performance
(setq gc-cons-threshold (* 50 1024 1024)
      gc-cons-percentage 0.6)
; using a custom file
(setq custom-file (cfg/path-s (format "emacs-custom--%s.el" cfg/profile)))
(when (file-exists-p custom-file) (load custom-file))

(custom-set-variables
 '(auto-save-default nil)
 '(create-lockfiles nil)
 '(make-backup-files nil)
 '(recentf-mode t) ; use the M-x recentf-open-files command
 '(recentf-save-file (cfg/path-s "recentf"))
 '(savehist-mode t)
 '(savehist-file (cfg/path-s "history"))
 '(use-short-answers t))

(custom-set-variables
 '(blink-cursor-blinks 3600)
 '(column-number-mode t)
 '(display-line-numbers t)
 '(global-display-line-numbers-mode t)
 '(fringe-mode 8 nil (fringe))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(scroll-step 1)
 '(tab-bar-mode t)   ; using tab bars
 '(tab-bar-show 100) ; hide if tabs less 100
 '(tool-bar-mode nil)
 '(global-hl-line-mode t) ;;; Подсветка текущей строки
;;  '(custom-safe-themes t)
 '(visible-bell t)) ; no blink please

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

(provide 'early-init)
;;; early-init.el ends here
