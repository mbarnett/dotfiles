;; Constants

(setq config-dir (expand-file-name "~/.emacs.d/"))
(setq backup-dir (concat config-dir "backups"))
(setq local-pkg-dir (concat config-dir "local-elisp"))
(setq pkg-dir (concat config-dir "elisp"))
(setq theme-dir (concat config-dir "themes"))


;; Load paths

(add-to-list 'load-path local-pkg-dir)
(add-to-list 'load-path pkg-dir)
(add-to-list 'load-path (concat pkg-dir "/w3m"))

;; My utilities

(require 'utility)


;; Smart Tab dynamic completions

(require 'smart-tab)
(global-smart-tab-mode 1)


;; SLIME config

(setq slime-lisp-implementations
      '((ccl ("/usr/local/bin/ccl"))
        (sbcl ("/usr/local/bin/sbcl"))
        (clisp ("/usr/local/bin/clisp"))))

(load (expand-file-name "~/.quicklisp/slime-helper.el"))


;; w3m Config

(require 'w3m-load)

(setq w3m-command "/usr/local/bin/w3m")
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)


;; Browse url at point with C-x m

(global-set-key "\C-xm" 'browse-url-at-point)


;; Vimpulse

;(require 'vimpulse)


;; Niceties

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

(global-font-lock-mode 1)           ; Syntax highlighting
(delete-selection-mode t)           ; Overwrite selections when you type
(scroll-bar-mode -1)                ; Remove the scollbar
(tool-bar-mode -1)                  ; Enable tool bar
(show-paren-mode t)                 ; Highlight the matching paren
(column-number-mode t)              ; Show column number in modeline
(set-fringe-style 'half)            ; Half-sized gutters on the edges
(set-backup-dir backup-dir)         ; Keep the filesystem tidy


;; Font and Appearance

(setq custom-theme-directory        ; Keep themes in their own sub-directories
      theme-dir)
(load-theme 'naquadah)
(set-font-if-exists "Menlo-12")     ; Sweet Menlo


;; Default frame size

(add-to-list 'default-frame-alist '(height . 49))
(add-to-list 'default-frame-alist '(width . 160))

(defun rcy-browse-url-default-macosx-browser (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((url
	 (if (aref (url-generic-parse-url url) 0)
	     url
	   (concat "http://" url))))
    (start-process (concat "open " url) nil "open" url)))
 
(setq browse-url-browser-function 'rcy-browse-url-default-macosx-browser)

    (setq mac-option-key-is-meta nil)
   (setq mac-command-key-is-meta t)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier nil)
(require 'unbound)



    (setq-default c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode nil)


(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(setq auto-mode-alist  (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.rhtml$" . html-mode) auto-mode-alist))

(modify-coding-system-alist 'file "\\.rb$" 'utf-8)
(modify-coding-system-alist 'file "\\.rhtml$" 'utf-8)

(add-hook 'ruby-mode-hook
          (lambda ()
            (set (make-local-variable 'indent-tabs-mode) 't)
            (set (make-local-variable 'tab-width) 2)
            ))


;; Serenity stuff

;; Serenity emacs key rebindings

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key (kbd "C-q") 'kill-region)
(global-set-key (kbd "M-q") 'kill-ring-save)



(global-set-key (kbd "M-/") 'comment-region)



(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

(global-set-key (kbd "C-]") 'indent-region)

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "C-o") 'open-next-line)

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one. 
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "M-o") 'open-previous-line)

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

   (add-to-list 'load-path "~/.emacs.d/elisp/textmate.el")
   (require 'textmate)
   (require 'peepopen)
   (textmate-mode)
(setq ns-pop-up-frames nil)
