;;; early-init.el --- Emacs init script  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;;; debug
(when init-file-debug
  (trace-function 'require)
  (trace-function 'load)
  ;; (trace-function 'package-initialize)
  ;; (trace-function 'custom-initialize-reset)
  ;; (require 'edebug)
  ;; (edebug-trace "CFG: start early-init.el")
  )

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
  (expand-file-name (format "target/%s/%s" cfg/profile arg) user-emacs-directory))
(defun cfg/path-u (arg)
  (expand-file-name (format "target/%s" arg) user-emacs-directory))
;;; micro tuning of Emacs
;; (setq frame-title-format '( "GNU/Emacs " cfg/title))
(setq frame-title-format
      '("GNU/Emacs"
        (:eval (if tab-bar-mode
                (concat " ___/ " (cdr (assq 'name (tab-bar--current-tab))) " \\___ ") ""))
         "<"cfg/title ">" cfg/title-tail))
;;;;; environment
(setq package-user-dir (cfg/path-t "elpa"))
(when (boundp 'native-comp-eln-load-path) ; set eln-cache dir
  (startup-redirect-eln-cache (cfg/path-t "eln-cache")))
;;;;; performance
(setq gc-cons-threshold (* 50 1024 1024)
      gc-cons-percentage 0.6)
;;;;; using a custom file
(setq custom-file (cfg/path-s (format "emacs-custom--%s.el" cfg/profile)))
(when (file-exists-p custom-file) (load custom-file))

;;;;; override custom
(custom-set-variables
 '(auto-save-default nil)
 '(auto-save-list-file-prefix (cfg/path-u "tmp-auto-save-list/.saves-"))
 '(create-lockfiles nil)
 '(make-backup-files nil)
 '(project-list-file (cfg/path-u "projects"))
 '(recentf-mode t) ; use the M-x recentf-open-files command
 '(recentf-save-file (cfg/path-u "recentf"))
 '(savehist-mode t)
 '(savehist-file (cfg/path-u "history"))
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
 '(visible-bell t)) ; no blink please

(setq package-enable-at-startup nil)    ; must have

(provide 'early-init)
;;; early-init.el ends here
