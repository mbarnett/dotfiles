(set-language-environment "utf-8")
(prefer-coding-system 'utf-8)
(setenv "LANG" "en_CA.UTF-8")

;;; Directories

(setq config-dir (expand-file-name "~/.emacs.d/")
      backup-dir (concat config-dir "backups")
      theme-dir (concat config-dir "themes/"))

(add-to-list 'custom-theme-load-path theme-dir)
(add-to-list 'custom-theme-load-path
             (concat theme-dir "/amelie-theme/"))

;;; Elpaca bootstrap. https://github.com/progfolio/elpaca

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;; Packages

(elpaca nyan-mode (when (display-graphic-p)
                    (setq nyan-wavy-trail t)
                    (nyan-mode t)
                    (nyan-start-animation)))

;; hide my numerous minor-modes, since it isn't very useful to see them

(elpaca rich-minority
        (setf rm-whitelist "a-fake-minor-mode-nothing-will-match")
        (rich-minority-mode 1))

(elpaca ace-jump-mode (global-set-key (kbd "C-j") 'ace-jump-mode))

;; Read in Mac env variables when launched via GUI

(elpaca exec-path-from-shell  (exec-path-from-shell-initialize))

(elpaca popwin
  (popwin-mode 1)
  (push "*elpaca-info*" popwin:special-display-config)
  (push "*elpaca-log*" popwin:special-display-config)
  (push "*Buffer List*" popwin:special-display-config)
  (push "*Warnings*" popwin:special-display-config)
  (push "*Help*" popwin:special-display-config))

(elpaca afternoon-theme)
(elpaca peacock-theme (load-theme 'peacock t))

;; vertico stack

(elpaca vertico
  (setq vertico-count 20
        vertico-cycle t
        vertico-resize t)
  (vertico-mode))

(elpaca orderless
  (setq completion-styles '(orderless basic)
        orderless-matching-styles '(orderless-flex)
        completion-category-overrides '((file (styles partial-completion)))
        completion-category-defaults nil))

(elpaca marginalia
  (setq marginalia-align 'right)
  (marginalia-mode))

(elpaca consult)

;;

(elpaca embark
  (global-set-key (kbd "C-;") 'embark-act))

(elpaca embark-consult)


;;

(elpaca vim-tab-bar
  (vim-tab-bar-mode))

;;

(elpaca vterm)

;;

(defun deadgrep-visit-result-other-tab ()
  "Goto the search result at point, opening in another window."
  (interactive)
  (deadgrep--visit-result #'find-file-other-tab))

(elpaca deadgrep
  (with-eval-after-load 'deadgrep
    (define-key deadgrep-mode-map (kbd "t") #'deadgrep-visit-result-other-tab))
  (setq deadgrep--context '(2 . 2)))

;; Autocomplete

(elpaca company
  (setq company-transformers '(company-sort-by-occurrence)
        company-idle-delay 0.1
        company-dabbrev-downcase nil
        company-clang-arguments '("-std=c++14" "-I/usr/local/include"))
  (add-hook 'prog-mode-hook #'company-mode)
  (add-hook 'prog-mode-hook (lambda ()
                              (local-set-key (kbd "<tab>") 'company-indent-or-complete-common)))
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-w") 'back-kill-or-kill-region)
    (define-key company-active-map (kbd "<tab>") 'company-complete-selection)))

(defun company-completion-styles (capf-fn &rest args)
  (let ((completion-styles '(basic)))
    (apply capf-fn args)))
(advice-add 'company-capf :around #'company-completion-styles)

;;

(elpaca projectile
  (setq projectile-project-search-path '("~/source" "~/source/eezy/"))
  (projectile-discover-projects-in-search-path))

(elpaca neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))

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
        `(( "." . ,dir)))
  (setq auto-save-file-name-transforms
          `((".*" ,dir t)))
  (setq auto-save-list-file-prefix
        (concat dir "/")))


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
          turn-on-pbcopy t))

(when (is-mac-gui)
  (setq ns-use-srgb-colorspace t
;        ns-pop-up-frames nil
        mac-option-key-is-meta t
        mac-option-modifier 'meta
        mac-command-modifier 'hyper
        mac-right-command-modifier 'meta))

;; spaces for tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq-default fill-column 120)

(setq-default truncate-lines t)
;(setq-default cursor-type 'bar)

(setq display-time-string-forms
      '((propertize (concat " " 12-hours ":" minutes am-pm))))
(display-time-mode 1)

(setq frame-title-format "%b")

(blink-cursor-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)  ;; Those long-form questions are annoying
(global-font-lock-mode 1)      ;; Syntax highlighting
(delete-selection-mode t)      ;; Overwrite selections when you type

(column-number-mode t)         ;; Show column number in modeline
;(transient-mark-mode 1)
;(cua-selection-mode t)         ;; CUA for regions only
(global-auto-revert-mode t)
(savehist-mode 1)
(electric-pair-mode 1)
;(xterm-mouse-mode 1)
                                        ;(menu-bar-mode -1)

(display-fill-column-indicator-mode 1)

;; Niceties

(setq backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 5
      kept-old-versions 1
      auto-save-default nil
  ;    vc-follow-symlinks nil
      newline-and-indent t
      ring-bell-function 'ignore
      show-paren-style 'expression
      split-width-threshold 9999
      history-length 1000
     ; mouse-autoselect-window t
 ;     neo-window-fixed-size nil
      shift-select-mode t)

;; remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(set-backup-dir backup-dir)    ;; Keep the filesystem tidy


;;; Interface

;; Default frame maximized

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Font

(if (is-mac-gui)
    (set-font-if-exists "MenloEmacs-18")) ; Menlo with custom height metrics hacked into it to render better in Emacs' overly-tight lineheight rendering

(if (is-linux-gui)
    (set-font-if-exists "Meslo LG S-10.25"))

;; Key rebindings

(global-set-key (kbd "C-w") 'back-kill-or-kill-region)

(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)
(global-set-key (kbd "C-a") 'move-start-of-line-or-prev-line)
(global-set-key (kbd "C-e") 'move-end-of-line-or-next-line)

; I always mess this up with C-x b, so live with it
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x C-b") 'consult-buffer)

(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

; disable weird two finger scroll buffer switching behaviour
(global-set-key [swipe-right] 'ignore)
(global-set-key [swipe-left] 'ignore)

(global-set-key (kbd "H-t") 'projectile-find-file)
(global-set-key (kbd "H-O") 'projectile-switch-project)
(global-set-key (kbd "H-|") 'neotree-toggle)

(global-set-key (kbd "H-S-<left>") 'tab-previous)
(global-set-key (kbd "H-S-<right>") 'tab-next)
(global-set-key (kbd "H-w") 'tab-close)
(global-set-key (kbd "H-n") 'tab-new)

(global-set-key (kbd "H-F") 'deadgrep)


(add-hook 'prog-mode-hook (lambda() (display-line-numbers-mode 1)))

;; hl-line

(global-hl-line-mode 1)

;; windmove

(windmove-default-keybindings 'hyper)
(setq windmove-wrap-around t)



;(global-set-key (kbd "H-v") 'cua-aste)

;(global-set-key (kbd "M-x") 'helm-M-x)
;(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
;(global-set-key (kbd "C-x b") 'helm-buffers-list)
;(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;(global-set-key [(meta shift p)] 'helm-projectile-switch-project)

;(global-set-key [(meta left)] 'tabbar-backward)
;(global-set-key [(meta right)] 'tabbar-forward)


;; Helm

; (setq completion-styles '(flex))

; (with-eval-after-load "helm-files"
;   (define-key helm-find-files-map (kbd "C-w")
;     'helm-find-files-up-one-level))

; (with-eval-after-load "helm"
;   (setq helm-split-window-in-side-p t)
;   (define-key helm-map (kbd "C-w")
;     'back-kill-or-kill-region))

;(eval-after-load 'helm-mode
 ;   '(add-to-list 'helm-completing-read-handlers-alist '(find-file)))


;; company-mode

;



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

; (require 'web-mode)

; (dolist (extension (list "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.djhtml\\'" "\\.mustache\\'" "\\.erb\\'" "\\.as[cp]x\\'"))
;   (add-to-list 'auto-mode-alist (cons extension 'web-mode)))

; (setq web-mode-markup-indent-offset 2)
; (setq web-mode-css-indent-offset 2)
; (setq web-mode-code-indent-offset 2)


; ;;use server-side comments
; (setq web-mode-comment-style 2)

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

; (add-hook 'cider-repl-mode-hook #'company-mode)
; (add-hook 'cider-mode-hook #'company-mode)


;; rainbow delimiters

; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; Projectile

; (setq projectile-indexing-method 'hybrid)
; (setq projectile-completion-system 'helm
;       projectile-switch-project-action 'helm-projectile)

; (projectile-global-mode)

; (define-key projectile-mode-map (kbd "<f6>") (lambda()
;                                                (interactive)
;                                                (treemacs)))

; (define-key projectile-mode-map (kbd "C-t")
;   'helm-projectile-find-file)

; (define-key projectile-mode-map [(meta t)]
;   'helm-projectile-find-file)

; ;(define-key projectile-mode-map [(meta shift f)]
; ;  'helm-projectile-ag)

; (setq projectile-globally-ignored-file-suffixes (append '(".xsl" ".xsd" ".pdf" ".jpeg" ".rdb" ".txt" ".md" ".html" ".lock" ".log" ".keep" ".elc" ".jpg" ".png") projectile-globally-ignored-file-suffixes))
; (setq projectile-globally-ignored-directories (append '(".*/backups" ".*/elpa" ".*/el-get" "logs") projectile-globally-ignored-directories))
; (setq projectile-globally-ignored-files (append '("\\..*" ".*~" ".*/.keep" ".*/*.jpg" ".*/*.png" ".*/*.lock" ".*/*.elc" ".*/*.pdf" ".*/*.log") projectile-globally-ignored-files))

; (helm-projectile-on)


; (setq forecast-api-key "e3df20dea7c305ca509f1f623b344dec")
; (setq forecast-coordinates "53.5444,-113.4909")


; (custom-set-variables
;  ;; custom-set-variables was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  '(custom-safe-themes
;    '("8ca8fbaeaeff06ac803d7c42de1430b9765d22a439efc45b5ac572c2d9d09b16" "3d81351b871668b10f7380d6c37142acda3cd6c485365b7b62a60725956c6550" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "cd2a93d7b63aff07b3565c1c95e461cb880f0b00d8dd6cdd10fa8ece01ffcfdf" default))
;  '(delete-selection-mode nil)
;  '(package-selected-packages
;    '(humanoid-themes pfuture memoize font-lock+ queue atom-one-dark-theme adjust-parens))
;  '(tabbar-separator '(1))
;  '(weatherline-location-id 5946768))
; (custom-set-faces
;  ;; custom-set-faces was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3bf336a4b70c64f133d98c0e72105b577ec13d8f6911c34ba97767ee29b0c558" "4a103344cde9cb0c44918d376018e37a1d26616a2cff02318560ff668ae2e6e5" "92bcce493131765b4e662252c6c81d8ccdd566c103d21eb6c0768567c942fe5b" "a81bc918eceaee124247648fc9682caddd713897d7fd1398856a5b61a592cb62" "c335adbb7d7cb79bc34de77a16e12d28e6b927115b992bccc109fb752a365c72" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
