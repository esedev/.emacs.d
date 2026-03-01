;;; early-init.el --- Emacs init script  -*- lexical-binding: t; -*-
;;; Commentary:

;; This file performs initial configuration initialization.
;; It's a good place to set up debugging options.

;; 1. Additional command line arguments starting with the
;;   --u-%name% prefix are processed. The values of these options
;;   ​​will be written to environment variables named EMAX_%NAME%.
;;   The current configuration uses the following environment variables:
;;   EMAX_PRF - Determines which initialization file will be loaded
;;   EMAX_THM - Overrides the Emacs color theme
;;   EMAX_JOB - A job script to run when Emacs starts
;;   example:  emacs --u-prf=devel --u-thm=wombat
;;             equivalent to the startup command
;;             env EMAX_PRF=devel EMAX_THM=wombat emacs

;; 2. Global variables that have values ​​for configuration settings are initialized
;;   cfg/profile - specifies which startup script will be used.
;;   Possible values: actual(default), built-in, craft, devel.

;; 3. Setting up paths, keep the configuration directory clean and tidy.
;;   Functions that return paths to configuration files are defined
;;   to ensure the configuration directory is kept in order.

;; 4. Tuning of performance

;;; Code:
;;;;; debug
(when init-file-debug
  (trace-function 'require)
  (trace-function 'load)
  ;; (trace-function 'package-initialize)
  ;; (trace-function 'custom-initialize-reset)
  ;; (require 'edebug)
  ;; (edebug-trace "CFG: start early-init.el")
  )

;;;;; processing extended command line arguments with '--u-' prefix
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

;;;;; initialize config variables
(defvar cfg/profile (intern (or (getenv "EMAX_PRF") "actual"))
  "User Emacs profile identity.")
(defvar cfg/job (getenv "EMAX_JOB") "Job identity, type string.")
(defvar cfg/directory user-emacs-directory "Path to config directory.")
(defvar cfg/frame-title (format "%s%s☀%s" cfg/profile
                                (if cfg/job (concat "-" cfg/job) "")
                                system-name) "Emacs frame title.")

;;;;; paths for clean and order
(defun cfg/path (arg)                   ; ~/.config/emacs/$arg
  "Return full path file ARG in `cfg/directory'."
  (expand-file-name arg cfg/directory))
(defun cfg/path-s (arg)                 ; ~/.config/emacs/save/$arg
  (cfg/path (concat "save/" arg)))

(setq user-emacs-directory (cfg/path "target"))

(defun cfg/path-u (arg)                 ; ~/.config/emacs/target/$arg
  "Return full path file ARG in `user-emacs-directory'."
  (expand-file-name arg user-emacs-directory))
(defun cfg/path-p (arg)                 ; ~/.config/emacs/target/prf--%name%/$arg
  "Return full profile path file ARG in `user-emacs-directory'."
  (cfg/path-u (format "prf--%s/%s" cfg/profile arg)))

(setq package-user-dir (cfg/path-p "elpa"))
(when (boundp 'native-comp-eln-load-path) ; set eln-cache dir
  (startup-redirect-eln-cache (cfg/path-p "eln-cache")))

(setq custom-file (cfg/path-s (format "custom-file-%s.el" cfg/profile)))

;;;;; micro tuning
;; (setq frame-title-format '( "GNU/Emacs " title))
(setq frame-title-format
      '("GNU/Emacs"
        (:eval (if tab-bar-mode
                   (concat " ___/ " (cdr (assq 'name (tab-bar--current-tab))) " \\___ ") ""))
        "<" cfg/frame-title ">"))

;;;;; performance
(setq gc-cons-threshold (* 64 1024 1024)) ; run gc after threshold
(setq read-process-output-max (* 1 1024 1024)) ; performance with language servers
(setenv "LSP_USE_PLISTS" "true") ; tree-sitter performance enhancement
;; (setq lsp-use-plists t)          ;

;;;;;
(setq package-enable-at-startup nil)    ; must be called explicitly (package-initialize)


(provide 'early-init)
;;; early-init.el ends here
