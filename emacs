;; Constants


;; Load paths

(add-to-list 'load-path (expand-file-name "~/.emacs.d/local-elisp"))


;; My utility functions

(require 'utility)


;; SLIME config

(setq inferior-lisp-program "/usr/local/bin/sbcl")
(load (expand-file-name "~/.quicklisp/slime-helper.el"))


;; Niceties

(setq backup-directory-alist 
      '(("." 
         . "~/.emacs.d/backups")))  ; Keep the filesystem tidy
(setq backup-by-copying t)          ; Don't clobber symlinks
(setq version-control t)            ; Keep multiple backups
(setq delete-old-versions t)        ; Clean out old backups
(setq kept-new-versions 5)          ; Keep the 5 newest versions
(setq kept-old-versions 1)          ; Don't keep the N oldest versions
(setq auto-save-default nil)        ; Only save when I say so
(setq inhibit-startup-message nil)  ; I like the startup message
(setq vc-follow-symlinks nil)       ; Ditch the error, we ain't using RVS here
(setq ring-bell-function            ; Less beepy, more sleepy
      'quieter-bell) 
(setq split-height-threshold 0)     ; We always want vertical splits  
(setq split-width-threshold nil)    ;

(setq-default tab-width 4)          ; Default tabs to 4 spaces
(setq-default indent-tabs-mode nil) ; Don't turn leading spaces into tabs

(fset 'yes-or-no-p 'y-or-n-p)       ; Those long-form questions are annoying

(delete-selection-mode t)           ; Overwrite selections when you type
(scroll-bar-mode -1)                ; Remove the scollbar
(tool-bar-mode -1)                  ; Enable tool bar
(show-paren-mode t)                 ; Highlight the matching paren
(column-number-mode t)              ; Show column number in modeline
(set-fringe-style 'half)            ; Half-sized gutters on the edges


;; Font and Appearance

(setq custom-theme-directory        ; Keep themes in their own sub-directories
      "~/.emacs.d/themes")
(load-theme 'naquadah)
(set-font-if-exists "Menlo-12")     ; Sweet Menlo


;; Default frame size

(add-to-list 'default-frame-alist '(height . 49))
(add-to-list 'default-frame-alist '(width . 160))