(set-language-environment "utf-8")
(prefer-coding-system 'utf-8)
(setenv "LANG" "en_CA.UTF-8")

;;; El-Get

;; my packages

(el-get-bundle all-the-icons)
(el-get-bundle ace-jump-mode)
(el-get-bundle cider)
(el-get-bundle company-mode)
(el-get-bundle dumb-jump)
(el-get-bundle emacs-async)
(el-get-bundle exec-path-from-shell)
(el-get-bundle fill-column-indicator)
(el-get-bundle helm)
(el-get-bundle helm-projectile)
(el-get-bundle humanoid-themes)
(el-get-bundle linum-off)
(el-get-bundle lsp-mode)
(el-get-bundle lsp-ui)
(el-get-bundle neotree)
(el-get-bundle nyan-mode)
(el-get-bundle popwin)
(el-get-bundle projectile)
(el-get-bundle rainbow-delimiters)
(el-get-bundle rich-minority)
(el-get-bundle rust-mode)
(el-get-bundle rvm)
(el-get-bundle s)
(el-get-bundle seq)
(el-get-bundle solarized-emacs)
(el-get-bundle tabbar)
(el-get-bundle web)
(el-get-bundle web-mode)
(el-get-bundle yasnippet)

;;; Directories

(setq config-dir (expand-file-name "~/.emacs.d/")
      backup-dir (concat config-dir "backups")
      local-lisp-dir (concat config-dir "local-lisp")
      theme-dir (concat config-dir "themes"))

(add-to-list 'load-path local-lisp-dir)

(add-to-list 'custom-theme-load-path theme-dir)

(require 'humanoid-themes)

(load-theme 'humanoid-dark t)
(set-face-attribute 'region nil :background "#134e69")

;;; Functions

(defun is-mac ()
  (eq system-type 'darwin))

(defun is-linux ()
  (eq system-type 'gnu/linux))

(defun is-gui ()
  (not (eq window-system nil)))

(defun is-mac-gui ()
  (and
   (is-mac)
   (is-gui)))

(defun is-linux-gui ()
  (and
   (is-linux)
   (is-gui)))

(defun set-font-if-exists (font)
  (let ((fallback-font "Courier-12"))
    (if (is-gui)
        (if (null (x-list-fonts font))
            (set-frame-font fallback-font nil t)
          (set-frame-font font nil t)))))


(defun set-backup-dir (dir)
  (setq backup-directory-alist
        `(( "." . ,dir))))


;; Like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun back-kill-or-kill-region (arg)
  "Kill the region if active, else backwards kill a word"
  (interactive "p")
  (if (and transient-mark-mode mark-active)
      (kill-region (region-beginning) (region-end))
    (delete-word (- arg))))

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


;;; Settings

;; Mac & GUI specific stuff

(when (is-mac)
    (setq browse-url-browser-function 'browse-url-default-macosx-browser
	  mac-option-key-is-meta nil
	  mac-command-key-is-meta t
	  mac-command-modifier 'meta
	  mac-option-modifier nil
	  turn-on-pbcopy t))

(when (is-mac-gui)
  (setq ns-use-srgb-colorspace t)
  (setq ns-pop-up-frames nil)
  (set-fringe-mode 0))

;; Read in Mac env variables when launched via GUI
(exec-path-from-shell-initialize)


;; Default frame size

(add-to-list 'default-frame-alist '(height . 52))
(add-to-list 'default-frame-alist '(width . 148))

;; spaces for tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq-default fill-column 120)

(setq-default truncate-lines t)
(setq-default cursor-type 'bar)

(setq display-time-string-forms
      '((propertize (concat " " 12-hours ":" minutes am-pm))))
(setq frame-title-format "%b")

(display-time-mode 1)
(blink-cursor-mode -1)
(scroll-bar-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)  ;; Those long-form questions are annoying
(global-font-lock-mode 1)      ;; Syntax highlighting
(delete-selection-mode t)      ;; Overwrite selections when you type
(show-paren-mode t)
(column-number-mode t)         ;; Show column number in modeline
(transient-mark-mode 1)
(cua-selection-mode t)         ;; CUA for regions only
(global-auto-revert-mode t)
(savehist-mode 1)
(electric-pair-mode 1)
(tool-bar-mode -1)


;; Niceties

(setq backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 5
      kept-old-versions 1
      auto-save-default nil
      vc-follow-symlinks nil
      newline-and-indent t
      ring-bell-function 'ignore
      show-paren-style 'expression
      split-width-threshold 9999
      history-length 1000
      mouse-autoselect-window t
      neo-window-fixed-size nil)


;; nuke trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(set-backup-dir backup-dir)    ;; Keep the filesystem tidy

;;; Interface

;; Font

(if (is-mac-gui)
	(set-font-if-exists "Menlo-15")) ;; Sweet Menlo

(if (is-linux-gui)
    (set-font-if-exists "Meslo LG S-10.25"))

;; Theme
;(if (display-graphic-p)
 ;   (require 'unfucked-solarize))


;; hide my numerous minor-modes, since it isn't very useful to see them

(setf rm-whitelist "a-fake-minor-mode-nothing-will-match")
(rich-minority-mode 1)


;; NYAN-MODE!!!111

(when (display-graphic-p)
    (setq nyan-wavy-trail t)
    (nyan-mode t)
    (nyan-start-animation))


;; windmove

(windmove-default-keybindings 'shift)
(setq windmove-wrap-around t)


;; Key rebindings

(global-set-key (kbd "C-w") 'back-kill-or-kill-region)

(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)
(global-set-key (kbd "C-a") 'move-start-of-line-or-prev-line)
(global-set-key (kbd "C-e") 'move-end-of-line-or-next-line)

(global-set-key (kbd "C-j") 'ace-jump-char-mode)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key [(meta shift p)] 'helm-projectile-switch-project)

(global-set-key [(meta left)] 'tabbar-backward)
(global-set-key [(meta right)] 'tabbar-forward)

(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)


;; Linum

(setq linum-disabled-modes-list '(ansi-term-mode wl-summary-mode compilation-mode dired-mode speedbar-mode direx:direx-mode))

(setq linum-format " %d ")
(global-linum-mode 1)

;; hl-line

(global-hl-line-mode 1)

;; PopWin and NeoTree integration

(require 'neotree)
(setq neo-autorefresh nil)
(setq neo-theme 'arrow)
(setq inhibit-compacting-font-caches t)

(require 'popwin)

(popwin-mode 1)


;; Tabbar

(require 'aquamacs-tabbar)

(tabbar-mode t)


;; Helm

;(setq helm-completion-style 'flex)
(setq completion-styles '(flex))

(with-eval-after-load "helm-files"
  (define-key helm-find-files-map (kbd "C-w")
    'helm-find-files-up-one-level))

(with-eval-after-load "helm"
  (setq helm-split-window-in-side-p t)
  (define-key helm-map (kbd "C-w")
    'back-kill-or-kill-region))

;(eval-after-load 'helm-mode
 ;   '(add-to-list 'helm-completing-read-handlers-alist '(find-file)))


;; company-mode

;; better completion sorting: see: https://github.com/company-mode/company-mode/issues/52
(setq company-transformers '(company-sort-by-occurrence)
      company-idle-delay 0.2
      company-dabbrev-downcase nil)

(setq company-clang-arguments '("-std=c++14" "-I/usr/local/include"))

(require 'company)

(define-key company-active-map (kbd "TAB") 'company-complete-selection)
(define-key company-active-map [tab] 'company-complete-selection)

(add-hook 'prog-mode-hook 'company-mode)


;;; Development

;; Basic code settings

(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\):*\\s-" 1 font-lock-warning-face t)))))

;; LSP

(setq lsp-ui-doc-position 'top)
(setq lsp-ui-doc-delay 1)

;; webmode

(require 'web-mode)

(dolist (extension (list "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.djhtml\\'" "\\.mustache\\'" "\\.erb\\'" "\\.as[cp]x\\'"))
  (add-to-list 'auto-mode-alist (cons extension 'web-mode)))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)


;;use server-side comments
(setq web-mode-comment-style 2)

;; ruby mode

(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

;; rust mode

(setq lsp-rust-server 'rust-analyzer)

;; Lisp stuff

(setq lisp-simple-loop-indentation 2
      lisp-loop-keyword-indentation 6
      lisp-loop-forms-indentation 9)

(font-lock-add-keywords
 'lisp-mode
 '(("[[:space:](]\\([0-9]+\\)[[:space:])]" 1 font-lock-constant-face)
   ("[[:space:](]\\(nil\\)[[:space:])]" 1 font-lock-constant-face)
   ("[[:space:](]\\(t\\)[[:space:])]" 1 font-lock-constant-face)))

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("[[:space:](]\\([0-9]+\\)[[:space:])]" 1 font-lock-constant-face)
   ("[[:space:](]\\(nil\\)[[:space:])]" 1 font-lock-constant-face)
   ("[[:space:](]\\(t\\)[[:space:])]" 1 font-lock-constant-face)))


;(load (expand-file-name "~/.quicklisp/slime-helper.el"))

;(setq slime-net-coding-system 'utf-8-utf) ;; unix-8 support for clozure
;(slime-setup '(slime-fancy slime-banner))

;(setq inferior-lisp-program "/usr/local/bin/ccl64")

;; clojure

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)


;; rainbow delimiters

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; Projectile

(setq projectile-indexing-method 'native)
(setq projectile-completion-system 'helm
      projectile-switch-project-action 'helm-projectile)

(projectile-global-mode)

(define-key projectile-mode-map (kbd "<f8>") (lambda()
                                               (interactive)
                                               (neotree-dir (projectile-project-root))))

(define-key projectile-mode-map (kbd "C-t")
  'helm-projectile-find-file)

(define-key projectile-mode-map [(meta t)]
  'helm-projectile-find-file)

;(define-key projectile-mode-map [(meta shift f)]
;  'helm-projectile-ag)

(setq projectile-globally-ignored-file-suffixes (append '(".xsl" ".xsd" ".pdf" ".jpeg" ".rdb" ".txt" ".md" ".html" ".lock" ".log" ".keep" ".elc" ".jpg" ".png") projectile-globally-ignored-file-suffixes))
(setq projectile-globally-ignored-directories (append '(".*/backups" ".*/elpa" ".*/el-get" "logs") projectile-globally-ignored-directories))
(setq projectile-globally-ignored-files (append '("\\..*" ".*~" ".*/.keep" ".*/*.jpg" ".*/*.png" ".*/*.lock" ".*/*.elc" ".*/*.pdf" ".*/*.log") projectile-globally-ignored-files))

(helm-projectile-on)


(setq forecast-api-key "e3df20dea7c305ca509f1f623b344dec")
(setq forecast-coordinates "53.5444,-113.4909")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8ca8fbaeaeff06ac803d7c42de1430b9765d22a439efc45b5ac572c2d9d09b16" "3d81351b871668b10f7380d6c37142acda3cd6c485365b7b62a60725956c6550" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "cd2a93d7b63aff07b3565c1c95e461cb880f0b00d8dd6cdd10fa8ece01ffcfdf" default)))
 '(package-selected-packages
   (quote
    (humanoid-themes pfuture memoize font-lock+ queue atom-one-dark-theme adjust-parens)))
 '(weatherline-location-id 5946768))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
