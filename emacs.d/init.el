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
(setq newline-and-indent t)         ; Autoindent open-*-lines

;; Settings for hippie-expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-expand-line
        try-complete-file-name-partially
        try-complete-file-name))
(setq smart-tab-using-hippie-expand t)

(fset 'yes-or-no-p 'y-or-n-p)       ; Those long-form questions are annoying

(global-font-lock-mode 1)           ; Syntax highlighting
(delete-selection-mode t)           ; Overwrite selections when you type
(scroll-bar-mode -1)                ; Remove the scollbar
(tool-bar-mode -1)                  ; Enable tool bar
(show-paren-mode t)                 ; Highlight the matching paren
(column-number-mode t)              ; Show column number in modeline
(set-fringe-style 'half)            ; Half-sized gutters on the edges
(set-backup-dir backup-dir)         ; Keep the filesystem tidy
(transient-mark-mode 1)             ; I prefer transient mark mode


;; Font and Appearance

(setq custom-theme-directory        ; Keep themes in their own sub-directories
      theme-dir)
(load-theme 'naquadah)
(set-font-if-exists "Menlo-12")     ; Sweet Menlo


;; Default frame size

(add-to-list 'default-frame-alist '(height . 49))
(add-to-list 'default-frame-alist '(width . 160))
 

;; Mac specific stuff

(setq browse-url-browser-function 'rcy-browse-url-default-macosx-browser)

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)


;; Key rebindings

(global-set-key (kbd "C-w") 'back-kill-or-kill-region)
(global-set-key (kbd "M-/") 'comment-region)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)


;; Basic code settings

(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)


;; Ruby mode settings

(require 'yaml-mode)

(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(setq auto-mode-alist  (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.rhtml$" . html-mode) auto-mode-alist))

(modify-coding-system-alist 'file "\\.rb$" 'utf-8)
(modify-coding-system-alist 'file "\\.rhtml$" 'utf-8)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))

(add-hook 'ruby-mode-hook
          (lambda ()
            (set (make-local-variable 'indent-tabs-mode) 't)
            (set (make-local-variable 'tab-width) 2)
            (global-set-key (kbd "C-l") 'insert-hashrocket)
            (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
            ))


;; Tabbar

(require 'tabbar)
(require 'tabbar-ruler)

(setq tabbar-buffer-groups-function
      (lambda ()
        (list "All Buffers")))

(setq tabbar-buffer-list-function
      (lambda ()
        (remove-if
         (lambda(buffer)
           (find (aref (buffer-name buffer) 0) " *"))
         (buffer-list))))


(setq EmacsPortable-global-tabbar 't) ; If you want tabbar


;; Speedbar

(require 'speedbar)
(require 'sr-speedbar)

(setq speedbar-use-images nil)
(setq sr-speedbar-right-side nil)
(speedbar-add-supported-extension ".rb")
(speedbar-add-supported-extension ".yml")
(setq sr-speedbar-auto-refresh nil)
(global-set-key [f1] 'sr-speedbar-toggle)


;; Smart Tab dynamic completions

(require 'smart-tab)
(global-smart-tab-mode 1)


;; eproject

(require 'eproject)

(defun eproject-grep (regexp)
  "Search all files in the current project for REGEXP."
  (interactive "sRegexp grep: ")
  (let* ((root (eproject-root))
         (default-directory root)
         (files (eproject-list-project-files-relative root)))
    (grep-compute-defaults)
    (lgrep regexp (combine-and-quote-strings files) root)))


;; peepopen

(require 'eproject-peepopen)
(setq ns-pop-up-frames nil)


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
(global-set-key "\C-xm" 'browse-url-at-point) ; Browse url at point with C-x m