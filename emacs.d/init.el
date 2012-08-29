(set-language-environment "utf-8")



;;; Constants

(setq fallback-font "Courier-12")
(setq config-dir (expand-file-name "~/.emacs.d/"))
(setq backup-dir (concat config-dir "backups"))
(setq local-pkg-dir (concat config-dir "local-elisp"))
(setq pkg-dir (concat config-dir "elisp"))
(setq theme-dir (concat config-dir "themes"))



;;; Load paths

(add-to-list 'load-path local-pkg-dir)
(add-to-list 'load-path pkg-dir)
(add-to-list 'load-path (concat pkg-dir "/w3m"))
(add-to-list 'load-path (concat local-pkg-dir "/shaved-yak"))



;;; Functions

(defun is-mac ()
  (eq system-type 'darwin))

(defun is-gui ()
  (not (eq window-system nil)))

(defun is-mac-gui ()
  (and 
   (is-mac)
   (is-gui)))


(defun set-font-if-exists (font)
  (if (is-gui)
      (if (null (x-list-fonts font))
          (set-default-font fallback-font) (set-default-font font))
    nil))


(defun quieter-bell ()
  (unless (memq this-command
    	'(isearch-abort abort-recursive-edit exit-minibuffer
              keyboard-quit mwheel-scroll down up next-line previous-line
              backward-char forward-char))
    (ding)))


(defun set-backup-dir (dir)
  (setq backup-directory-alist
        `(( "." . ,dir))))


; Like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

; Like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one. 
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))


(defun back-kill-or-kill-region (arg)
  "Kill the region if active, else backwards kill a word"
  (interactive "p")
  (if (and transient-mark-mode mark-active)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))


; For launching the default browser on os x
(defun rcy-browse-url-default-macosx-browser (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((url
	 (if (aref (url-generic-parse-url url) 0)
	     url
	   (concat "http://" url))))
    (start-process (concat "open " url) nil "open" url)))


(defun insert-hashrocket ()
  "Inserts a hashrocket"
  (interactive)
  (insert " => "))


(defun move-end-of-line-or-next-line ()
  (interactive)
  (if (eolp)
      (next-line)
      (move-end-of-line nil)))


(defun move-start-of-line-or-prev-line ()
  (interactive)
  (if (bolp)
      (previous-line)
      (move-beginning-of-line nil)))


; Hunt down the name of the Slime REPL buffer, if it exists
(defun find-slime-repl-buffer-name ()
  (let ((buffers (mapcar (function buffer-name) (buffer-list)))
        (candidate nil))
    (while buffers
      (setq candidate (car buffers))
      (if (string-match "^\*slime-repl.*\*$" candidate)
          (setq buffers nil)
        (progn
          (setq candidate nil)
          (setq buffers (cdr buffers)))))
    candidate))

(defun smart-slime-repl-switch ()
  (interactive)
  (let
      ((slime-repl-buffer (find-slime-repl-buffer-name))
       (current-buffer-name (buffer-name (current-buffer))))
    (if (and slime-repl-buffer (not (string= slime-repl-buffer current-buffer-name)))
        (switch-to-buffer (find-slime-repl-buffer-name))
      (switch-to-buffer (other-buffer (current-buffer) 1)))))



;;; Settings


;; Niceties

(setq backup-by-copying t)                  ; Don't clobber symlinks                   
(setq version-control t)                    ; Keep multiple backups                    
(setq delete-old-versions t)                ; Clean out old backups                    
(setq kept-new-versions 5)                  ; Keep the 5 newest versions               
(setq kept-old-versions 1)                  ; Don't keep the N oldest versions         
(setq auto-save-default nil)                ; Only save when I say so                  
(setq inhibit-startup-message nil)          ; I like the startup message               
(setq vc-follow-symlinks nil)               ; Ditch the error, we ain't using RVS here 
(setq ring-bell-function                    ; Less beepy, more sleepy                  
      'quieter-bell)                                                                   
(setq split-height-threshold 0)             ; We always want vertical splits           
(setq split-width-threshold nil)            ;                                          
                                                                                       
(setq-default tab-width 4)                  ; Default tabs to 4 spaces                 
(setq-default indent-tabs-mode nil)         ; Don't turn leading spaces into tabs      
(setq newline-and-indent t)                 ; Autoindent open-*-lines

(fset 'yes-or-no-p 'y-or-n-p)               ; Those long-form questions are annoying

(global-font-lock-mode 1)                   ; Syntax highlighting
(delete-selection-mode t)                   ; Overwrite selections when you type
(show-paren-mode t)                         ; Highlight the matching paren
(column-number-mode t)                      ; Show column number in modeline
(set-backup-dir backup-dir)                 ; Keep the filesystem tidy
(transient-mark-mode 1)                     ; I prefer transient mark mode
;(blink-cursor-mode -1)
(global-linum-mode 1)
(cua-selection-mode t)                      ; CUA for regions only



;;; Interface


;; Font and Appearance

(setq custom-theme-directory theme-dir)      ; Keep themes in their own sub-directories
(load-theme 'naquadah t)
(set-font-if-exists "Menlo-12")              ; Sweet Menlo


;; Default frame size

(add-to-list 'default-frame-alist '(height . 49))
(add-to-list 'default-frame-alist '(width . 160))


;; Key rebindings

(global-set-key (kbd "C-w") 'back-kill-or-kill-region)  
(global-set-key (kbd "M-/") 'comment-region)            
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)      
(global-set-key (kbd "C-o") 'open-next-line)            
(global-set-key (kbd "M-o") 'open-previous-line)        
(global-set-key (kbd "C-a") 'move-start-of-line-or-prev-line)
(global-set-key (kbd "C-e") 'move-end-of-line-or-next-line)
(global-set-key (kbd "C-;") 'smart-slime-repl-switch)   
(global-set-key (kbd "M-<RET>") 'cua-set-rectangle-mark)


;; Mac & GUI specific stuff

(if (is-gui)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (set-fringe-style 'half)))

(if (not (is-gui))
    (menu-bar-mode -1))

(if (is-mac)
    (progn
      (setq browse-url-browser-function 'rcy-browse-url-default-macosx-browser)
      (setq mac-option-key-is-meta nil)
      (setq mac-command-key-is-meta t)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)))

(if (is-mac-gui)
    (progn
      (ns-set-resource nil "ApplePressAndHoldEnabled" "NO")
      (setq ns-pop-up-frames nil)))


;; w3m Config

(require 'w3m-load)

(setq w3m-command "/usr/local/bin/w3m")
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(global-set-key "\C-xm" 'browse-url-at-point) ; Browse url at point with C-x m


;; Nyan mode

(require 'nyan-mode)

(nyan-mode 1)


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

(setq EmacsPortable-global-tabbar 't)


;; Speedbar

(require 'speedbar)
(require 'sr-speedbar)

(setq speedbar-use-images nil)
(setq sr-speedbar-right-side nil)
(speedbar-add-supported-extension ".rb")
(speedbar-add-supported-extension ".yml")
(speedbar-add-supported-extension ".lisp")
(speedbar-add-supported-extension ".asd")
(setq sr-speedbar-auto-refresh nil)
(global-set-key [f1] 'sr-speedbar-toggle)


;; Smart Tab dynamic completions

(require 'smart-tab)
(global-smart-tab-mode 1)


;; Settings for hippie-expand

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-expand-line
        try-complete-file-name-partially
        try-complete-file-name))

(setq smart-tab-using-hippie-expand t)



;;; Development


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

(setq ruby-deep-indent-paren nil)

(add-hook 'ruby-mode-hook
          (lambda ()
            (set (make-local-variable 'indent-tabs-mode) 't)
            (set (make-local-variable 'tab-width) 2)
            (global-set-key (kbd "C-l") 'insert-hashrocket)
            (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))


;; Lisp stuff

(setq lisp-simple-loop-indentation 2
      lisp-loop-keyword-indentation 6
      lisp-loop-forms-indentation 9)


;; ObjC

(setq auto-mode-alist (cons '("\\.mm$" . objc-mode) auto-mode-alist))


;; Go

(require 'go-mode-load)


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


;; SLIME config

(setq slime-lisp-implementations
      '((ccl ("/usr/local/bin/ccl64"))
        (ccl32 ("/usr/local/bin/ccl"))
        (sbcl ("/usr/local/bin/sbcl"))
        (clisp ("/usr/local/bin/cmucl"))
        (clisp ("/usr/local/bin/load"))))

(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq slime-net-coding-system 'utf-8-unix) ; utf-8 support for clozure 
(slime-setup '(slime-fancy slime-banner))


;; Shaved Yak

(require 'shaved-yak)

(global-set-key (kbd "M-p") 'shaved-yak-goto-file)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (naquadah)))
 '(custom-safe-themes (quote ("7312b8cec90bc1c59cd1470a49a7ad23adae013ab525119b24b4e9ec1a3dfff8" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
