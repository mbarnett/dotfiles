(set-language-environment "utf-8")

;;; Load el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq my-el-get-packages
      (append
       '(ace-jump-mode
         ack-and-a-half
         auto-complete
         cider
         dash-at-point
         direx
         el-get
         exec-path-from-shell
         grizzl
         magit
         markdown-mode
         nyan-mode
         pbcopy
         popwin
         projectile
         rainbow-delimiters
         rainbow-mode
         switch-window
         tabbar
         web-mode)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-el-get-packages)



;;; Directories

(setq config-dir (expand-file-name "~/.emacs.d/"))
(setq backup-dir (concat config-dir "backups"))
(setq local-pkg-dir (concat config-dir "local-elisp"))
(setq theme-dir (concat config-dir "themes/"))
(setq solarized-dir (concat theme-dir "emacs-color-theme-solarized"))
(setq w3m-dir (concat config-dir "w3m"))



;;; Load paths

(add-to-list 'load-path w3m-dir)
(add-to-list 'load-path local-pkg-dir)
(add-to-list 'load-path (concat local-pkg-dir "/shaved-yak"))
(add-to-list 'custom-theme-load-path theme-dir)
(add-to-list 'custom-theme-load-path solarized-dir)


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
  (let ((fallback-font "Courier-12"))
  (if (is-gui)
      (if (null (x-list-fonts font))
          (set-default-font fallback-font) (set-default-font font))
    nil)))


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

;; Mac & GUI specific stuff

(if (is-gui)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)))

(if (not (is-gui))
    (menu-bar-mode -1))

(if (is-mac)
    (progn
      (setq browse-url-browser-function 'rcy-browse-url-default-macosx-browser)
      (setq mac-option-key-is-meta nil)
      (setq mac-command-key-is-meta t)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)
      (turn-on-pbcopy)))

(if (is-mac-gui)
    (progn
      (ns-set-resource nil "ApplePressAndHoldEnabled" "NO")
      (setq ns-pop-up-frames nil)
      (set-fringe-mode 0)
      ;; Read in Mac env variables when launched via GUI
      (exec-path-from-shell-initialize)))


;; Niceties

(setq backup-by-copying t)                  ; Don't clobber symlinks
(setq version-control t)                    ; Keep multiple backups
(setq delete-old-versions t)                ; Clean out old backups
(setq kept-new-versions 5)                  ; Keep the 5 newest versions
(setq kept-old-versions 1)                  ; Don't keep the N oldest versions
(setq auto-save-default nil)                ; Only save when I say so
(setq inhibit-startup-message t)
(setq vc-follow-symlinks nil)               ; Ditch the error, we ain't using RVS here
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)                  ; Default tabs to 4 spaces
(setq newline-and-indent t)                 ; Autoindent open-*-lines


(fset 'yes-or-no-p 'y-or-n-p)               ; Those long-form questions are annoying

(global-font-lock-mode 1)                   ; Syntax highlighting
(delete-selection-mode t)                   ; Overwrite selections when you type
(show-paren-mode t)
(setq show-paren-style 'expression)
(column-number-mode t)                      ; Show column number in modeline
(set-backup-dir backup-dir)                 ; Keep the filesystem tidy
(transient-mark-mode 1)                     ; I prefer transient mark mode
(cua-selection-mode t)                      ; CUA for regions only
(desktop-save-mode 1)
(global-auto-revert-mode t)
(setq mouse-wheel-progressive-speed nil)
(setq ring-bell-function 'ignore)


(set-default 'cursor-type 'bar)

(add-to-list 'same-window-buffer-names "*inferior-lisp*")
(add-to-list 'same-window-buffer-names "*ack*")

; nuke trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;; Interface


;; Font and Appearance

(set-font-if-exists "Menlo-12")              ; Sweet Menlo
(load-theme 'solarized-dark t)


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
(global-set-key (kbd "C-j") 'ace-jump-word-mode)
(global-set-key (kbd "C-f") 'forward-word)
(global-set-key (kbd "C-b") 'backward-word)
(global-set-key (kbd "M-f") 'forward-char)
(global-set-key (kbd "M-b") 'backward-char)


;; Line numbers

(setq linum-format " %d ")

; for linum-off
(setq linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode dired-mode speedbar-mode))

(global-linum-mode 1)


;; w3m Config

(setq w3m-command "/usr/local/bin/w3m")
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(global-set-key "\C-xm" 'browse-url-at-point) ; Browse url at point with C-x m


;; Nyan mode

(nyan-mode 1)


;; Tabbar

(require 'tabbar)

(setq tabbar-buffer-groups-function
      (lambda ()
        (list "All Buffers")))

(setq tabbar-buffer-list-function
      (lambda ()
        (remove-if
         (lambda(buffer)
           (let ((bname (buffer-name buffer)))
             (or
              (and
               (find  (aref bname 0) " *")
               (not (member bname '("*scratch*" "*ack*")))
               (not (string-prefix-p "*slime-repl" bname)))
              (eq (buffer-local-value 'major-mode buffer) 'direx:direx-mode))))
         (buffer-list))))

(setq tabbar-cycle-scope (quote tabs))
(setq table-time-before-update 0.1)
(setq tabbar-use-images t)

;; place a space around the label to make it looks less crowd
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value (concat " " (concat ad-return-value " "))))

(global-set-key (kbd "M-[ d") 'tabbar-backward)
(global-set-key (kbd "M-[ c") 'tabbar-forward)
(global-set-key [C-left] 'tabbar-backward)
(global-set-key [C-right] 'tabbar-forward)

(tabbar-mode t)


;; Smart Tab dynamic completions

;(require 'smart-tab)
;(global-smart-tab-mode 1)

;; Autocomplete

(require 'auto-complete-config)
 (add-to-list 'ac-dictionary-directories
     "~/.emacs.d/.cask/24.3.50.1/elpa/auto-complete-20130724.1750/dict")
 (ac-config-default)
 (setq ac-ignore-case nil)
 (add-to-list 'ac-modes 'ruby-mode)
 (add-to-list 'ac-modes 'web-mode)


;; popwin

(require 'popwin)

(push '(direx:direx-mode :position left :width 25 :dedicated t :stick t)
      popwin:special-display-config)

(push '("magit" :position bottom :height 25 :dedicated t :stick f :regexp t)
      popwin:special-display-config)

(push '(ack-and-a-half-mode :position bottom :height 20 :dedicated t :stick f)
      popwin:special-display-config)

(popwin-mode 1)


;; Projectile

(setq projectile-indexing-method 'alien)
(setq projectile-completion-system 'grizzl)
(projectile-global-mode)

(define-key projectile-mode-map (kbd "M-p")
    'projectile-find-file)

(define-key projectile-mode-map [(meta shift f)]
  'projectile-ack)

(define-key projectile-mode-map [(meta shift r)]
  'projectile-replace)


;; Direx

(define-key projectile-mode-map (kbd "C-c C-c")
  (lambda ()
    (interactive)
    (direx:find-directory-other-window (projectile-project-root))))

; Direx hard codes use of find-item internally when selecting files with RET
; I'd much rather it always use find-item-other-window, so let's override it
(defun direx:find-item (&optional item)
  (direx:find-item-other-window item))


;; hl-line

(global-hl-line-mode 1)


;;; Development


;; Basic code settings

(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)


;; Ruby mode settings

(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(setq auto-mode-alist  (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.rhtml$" . html-mode) auto-mode-alist))

(modify-coding-system-alist 'file "\\.rb$" 'utf-8)
(modify-coding-system-alist 'file "\\.rhtml$" 'utf-8)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Gemfile$" . ruby-mode))

(setq ruby-deep-indent-paren nil)

(add-hook 'ruby-mode-hook
          (lambda ()
            (set (make-local-variable 'tab-width) 2)
            (local-set-key (kbd "C-l") 'insert-hashrocket)
            (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))


;; Lisp stuff

(setq lisp-simple-loop-indentation 2
      lisp-loop-keyword-indentation 6
      lisp-loop-forms-indentation 9)

(setq slime-lisp-implementations
      '((ccl ("/usr/local/bin/ccl64"))
        (ccl32 ("/usr/local/bin/ccl"))
        (sbcl ("/usr/local/bin/sbcl"))
        (cmucl ("/usr/local/bin/lisp"))
        (clisp ("/usr/local/bin/clisp"))))

(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq slime-net-coding-system 'utf-8-unix) ; utf-8 support for clozure
(slime-setup '(slime-fancy slime-banner))

(font-lock-add-keywords
 'lisp-mode
 '(("[[:space:](]\\([0-9]+\\)[[:space:])]" 1 font-lock-constant-face)
   ("[[:space:](]\\(nil\\)[[:space:])]" 1 font-lock-constant-face)
   ("[[:space:](]\\(t\\)[[:space:])]" 1 font-lock-constant-face)))



;; Rainbow Delimiters

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; ObjC

(setq auto-mode-alist (cons '("\\.mm$" . objc-mode) auto-mode-alist))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nyan-animate-nyancat t)
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default))))
(custom-set-faces
 '(tabbar-default ((t (:inherit variable-pitch :background "brightyellow" :foreground "black" :weight bold))))
 '(tabbar-selected ((t (:inherit tabbar-default :foreground "grey" :background "black"))))
 '(tabbar-unselected ((t (:background "black" :inherit variable-pitch))))
 '(col-highlight ((t (:background "black"))))
 '(font-lock-constant-face ((t (:foreground "#af005f")))))
