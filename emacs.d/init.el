(set-language-environment "utf-8")
(prefer-coding-system 'utf-8)
(setenv "LANG" "en_CA.UTF-8")

;;; Load el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(el-get-bundle exec-path-from-shell)


;;; Directories

(setq config-dir (expand-file-name "~/.emacs.d/")
      backup-dir (concat config-dir "backups"))


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

(if (is-mac-gui)
    (progn
      (ns-set-resource nil "ApplePressAndHoldEnabled" "NO")
      (setq ns-use-srgb-colorspace t)
      (setq ns-pop-up-frames nil)
      (set-fringe-mode 0)
      ;; Read in Mac env variables when launched via GUI
      (exec-path-from-shell-initialize)))

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

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; This causes the current time in the mode line to be displayed in
 ;; `egoge-display-time-face' to make it stand out visually.
(setq display-time-string-forms
      '((propertize (concat " " 12-hours ":" minutes am-pm)
 		    'face 'egoge-display-time)))

(fset 'yes-or-no-p 'y-or-n-p)               ; Those long-form questions are annoying

(global-font-lock-mode 1)                   ; Syntax highlighting
(delete-selection-mode t)                   ; Overwrite selections when you type

(show-paren-mode t)
(column-number-mode t)                      ; Show column number in modeline
(set-backup-dir backup-dir)                 ; Keep the filesystem tidy
(transient-mark-mode 1)
(cua-selection-mode t)                      ; CUA for regions only
;(desktop-save-mode 1)
(global-auto-revert-mode 1)
(savehist-mode 1)
(electric-pair-mode 1)

; nuke trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Interface


;; Font and Appearance

(set-font-if-exists "Menlo-12")              ; Sweet Menlo

;; Default frame size

(add-to-list 'default-frame-alist '(height . 52))
(add-to-list 'default-frame-alist '(width . 148))


;; Key rebindings

(global-set-key (kbd "C-w") 'back-kill-or-kill-region)
(global-set-key (kbd "M-/") 'comment-region)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)
(global-set-key (kbd "C-a") 'move-start-of-line-or-prev-line)
(global-set-key (kbd "C-e") 'move-end-of-line-or-next-line)
(global-set-key (kbd "M-<RET>") 'cua-set-rectangle-mark)
(global-set-key (kbd "C-j") 'ace-jump-word-mode)
(global-set-key (kbd "C-f") 'forward-word)
(global-set-key (kbd "C-b") 'backward-word)
(global-set-key (kbd "M-f") 'forward-char)
(global-set-key (kbd "M-b") 'backward-char)



;; Linum

; for linum-off
(setq linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode dired-mode speedbar-mode direx:direx-mode))

(setq linum-format " %d ")
(global-linum-mode 1)

;; hl-line

(global-hl-line-mode 1)


;;; Development


;; Basic code settings

(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)
