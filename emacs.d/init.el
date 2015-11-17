(set-language-environment "utf-8")
(prefer-coding-system 'utf-8)
(setenv "LANG" "en_CA.UTF-8")

;; add melpa to elpa for el-get

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;; Load el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(setq el-get-emacswiki-base-url "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/")

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(setq el-get-sources '((:name atom-one-dark-theme :type elpa)))

;; my packages

(el-get-bundle aaron-em/weatherline-mode.el)
(el-get-bundle atom-one-dark-theme)
(el-get-bundle cider)
(el-get-bundle company-mode)
(el-get-bundle exec-path-from-shell)
;;(el-get-bundle flyspell)
(el-get-bundle helm)
(el-get-bundle linum-off)
(el-get-bundle neotree)
(el-get-bundle nyan-mode)
(el-get-bundle popwin)
(el-get-bundle projectile)
(el-get-bundle rainbow-delimiters)
(el-get-bundle solarized-emacs)
(el-get-bundle tabbar)
(el-get-bundle wanderlust)
(el-get-bundle web-mode)

;;; Directories

(setq config-dir (expand-file-name "~/.emacs.d/")
      backup-dir (concat config-dir "backups")
      local-lisp-dir (concat config-dir "local-lisp"))

(add-to-list 'load-path local-lisp-dir)

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
            (set-frame-font fallback-font nil t) (set-frame-font font nil t)))))


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


(if (is-mac)
    (progn
      (setq browse-url-browser-function 'browse-url-default-macosx-browser
            mac-option-key-is-meta nil
            mac-command-key-is-meta t
            mac-command-modifier 'meta
            mac-option-modifier nil
            turn-on-pbcopy)))

(when (is-mac-gui)
  (ns-set-resource nil "ApplePressAndHoldEnabled" "NO")
  (setq ns-use-srgb-colorspace t)
  (setq ns-pop-up-frames nil)
  (set-fringe-mode 0)
  ;; Read in Mac env variables when launched via GUI
  (exec-path-from-shell-initialize))

;; Niceties

(setq backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 5
      kept-old-versions 1
      auto-save-default nil
      vc-follow-symlinks nil
      newline-and-indent t
      mouse-wheel-progressive-speed nil
      ring-bell-function 'ignore
      show-paren-style 'expression
      split-width-threshold 9999
      history-length 1000)

                                        ; spaces for tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq display-time-string-forms
      '((propertize (concat " " 12-hours ":" minutes am-pm))))

(display-time-mode 1)
(blink-cursor-mode -1)
(scroll-bar-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)               ; Those long-form questions are annoying

(global-font-lock-mode 1)                   ; Syntax highlighting
(delete-selection-mode t)                   ; Overwrite selections when you type

(show-paren-mode t)

(column-number-mode t)                      ; Show column number in modeline
(set-backup-dir backup-dir)                 ; Keep the filesystem tidy
(transient-mark-mode 1)
(cua-selection-mode t)                      ; CUA for regions only
(global-auto-revert-mode t)
(savehist-mode 1)
(electric-pair-mode 1)
(tool-bar-mode -1)

                                        ; nuke trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;; Interface

;; Font

(if (is-mac-gui)
	(set-font-if-exists "Menlo-12"))              ; Sweet Menlo

(if (is-linux-gui)
    (set-font-if-exists "Meslo LG S-10.25"))

;; Theme
(require 'unfucked-solarize)

;;(require 'better-atom-one-dark)

;; Default frame size

(add-to-list 'default-frame-alist '(height . 52))
(add-to-list 'default-frame-alist '(width . 148))

;; NYAN-MODE!!!111

(setq nyan-wavy-trail t)
(nyan-mode t)
(nyan-start-animation)

;; weather

(require 'weatherline-mode)

(setq weatherline-location "Edmonton,CA")
(setq weatherline-location-id "5946768")
(setq weatherline-units "metric")
(setq weatherline-temperature-indicator "°C")
(setq weatherline-lighter-include-humidity nil)
(weatherline-mode t)

;; windmove

(windmove-default-keybindings 'meta)
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

(global-set-key [(meta shift left)] 'tabbar-backward)
(global-set-key [(meta shift right)] 'tabbar-forward)

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
(require 'popwin)


(when neo-persist-show
  (add-hook 'popwin:before-popup-hook
            (lambda () (setq neo-persist-show nil)))
  (add-hook 'popwin:after-popup-hook
            (lambda () (setq neo-persist-show t))))



(popwin-mode 1)

;; Tabbar

;; place a space around the label to make it looks less crowd
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value (concat " " ad-return-value " ")))

(tabbar-mode t)

;; Helm

(with-eval-after-load "helm-files"
  (define-key helm-find-files-map (kbd "C-w")
    'helm-find-files-up-one-level))

(with-eval-after-load "helm-grep"
  (dolist (map (list helm-grep-map helm-pdfgrep-map))
    (define-key map (kbd "C-w") 'back-kill-or-kill-region)))

(with-eval-after-load "helm"
  (setq helm-split-window-in-side-p t
        helm-M-x-fuzzy-match t)
  (define-key helm-map (kbd "C-w")
    'back-kill-or-kill-region)

  (defun find-marked-candidates ()
    (cl-dolist (cand (helm-marked-candidates))
      (find-file-noselect cand)))

  (defun helm-grep-action (candidate &optional where mark)
  "Define a default action for `helm-do-grep' on CANDIDATE.
WHERE can be one of other-window, elscreen, other-frame."
  (let* ((split        (helm-grep-split-line candidate))
         (lineno       (string-to-number (nth 1 split)))
         (loc-fname        (or (with-current-buffer
                                   (if (eq major-mode 'helm-grep-mode)
                                       (current-buffer)
                                       helm-buffer)
                                 (get-text-property (point-at-bol) 'help-echo))
                               (car split)))
         (tramp-method (file-remote-p (or helm-ff-default-directory
                                          default-directory) 'method))
         (tramp-host   (file-remote-p (or helm-ff-default-directory
                                          default-directory) 'host))
         (tramp-prefix (concat "/" tramp-method ":" tramp-host ":"))
         (fname        (if tramp-host
                           (concat tramp-prefix loc-fname) loc-fname)))
    (cl-case where
      (other-window (find-file-other-window fname))
      (elscreen     (helm-elscreen-find-file fname))
      (other-frame  (find-file-other-frame fname))
      (grep         (helm-grep-save-results-1))
      (pdf          (if helm-pdfgrep-default-read-command
                        (helm-pdfgrep-action-1 split lineno (car split))
                      (find-file (car split)) (doc-view-goto-page lineno)))
      (t            (find-marked-candidates)))
    (unless (or (eq where 'grep) (eq where 'pdf))
      (helm-goto-line lineno))
    (when mark
      (set-marker (mark-marker) (point))
      (push-mark (point) 'nomsg))
    ;; Save history
    (unless (or helm-in-persistent-action
                (eq major-mode 'helm-grep-mode)
                (string= helm-pattern ""))
      (setq helm-grep-history
            (cons helm-pattern
                  (delete helm-pattern helm-grep-history)))
      (when (> (length helm-grep-history)
               helm-grep-max-length-history)
        (setq helm-grep-history
              (delete (car (last helm-grep-history)))))))))


;; flyspell

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;;(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; company-mode

;; better completion sorting: see: https://github.com/company-mode/company-mode/issues/52
(setq company-transformers '(company-sort-by-occurrence)
      company-idle-delay 0.2
      company-dabbrev-downcase nil)

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
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\)\\s-" 1 font-lock-warning-face t)))))

;; webmode

(require 'web-mode)

(dolist (extension (list "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.djhtml\\'" "\\.mustache\\'" "\\.erb\\'" "\\.as[cp]x\\'"))
  (add-to-list 'auto-mode-alist (cons extension 'web-mode)))


                                        ;use server-side comments
(setq web-mode-comment-style 2)

;; ruby mode

(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

                                        ; fix ruby's screwy indentation

(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (or (eq (char-after) ?\))
                       (and (eq (char-after) ?\})
                            (eq (char-after (1+ (point))) ?\))))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0)
        (forward-char offset)))))

(setq ruby-deep-indent-paren nil
      ruby-deep-indent-paren-style nil
      ruby-deep-arglist nil
      ruby-use-smie nil)


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


(load (expand-file-name "~/.quicklisp/slime-helper.el"))

(setq slime-net-coding-system 'utf-8-unix) ; utf-8 support for clozure
(slime-setup '(slime-fancy slime-banner))

(setq inferior-lisp-program "/home/matt/bin/ccl")


;; rainbow delimiters

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Projectile

(setq projectile-completion-system 'helm
      projectile-switch-project-action 'helm-projectile)

(projectile-global-mode)

(define-key projectile-mode-map (kbd "<f8>") (lambda()
                                               (interactive)
                                               (neotree-dir (projectile-project-root))))

(define-key projectile-mode-map (kbd "C-t")
  'helm-projectile-find-file)

(define-key projectile-mode-map [(meta shift f)]
  'helm-projectile-grep)

(helm-projectile-on)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "cd2a93d7b63aff07b3565c1c95e461cb880f0b00d8dd6cdd10fa8ece01ffcfdf" default)))
 '(weatherline-location-id 5946768))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
