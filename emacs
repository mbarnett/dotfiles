;; SLIME config

(setq inferior-lisp-program "/usr/local/bin/sbcl")
(load (expand-file-name "~/.quicklisp/slime-helper.el"))

;; Nicities

(setq make-backup-files nil)        ; No obnoxious backups
(setq auto-save-default nil)        ; Only save when I say so
(setq-default tab-width 4)          ; Default tabs to 4 spaces
(setq-default indent-tabs-mode nil) ; Don't turn leading spaces into tabs
(setq inhibit-startup-message nil)  ; I like the startup message

;(fset 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style -1)
(tooltip-mode -1)

;(set-frame-font "Menlo-16")
;(load-theme 'tango)