;;; My local utility functions

(setq fallback-font "Courier-12")

;; Portably specify default fonts like Apple Menlo

(defun set-font-if-exists (font)
  (if (not (eq window-system nil))
      (if (null (x-list-fonts font))
          (set-default-font fallback-font) (set-default-font font))
    nil))


;; A quieter bell function

(defun quieter-bell ()
  (unless (memq this-command
    	'(isearch-abort abort-recursive-edit exit-minibuffer
              keyboard-quit mwheel-scroll down up next-line previous-line
              backward-char forward-char))
    (ding)))


;; Set the backup dir more cleanly

(defun set-backup-dir (dir)
  (setq backup-directory-alist
        `(( "." . ,dir))))


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


;; Behave like vi's O command

(defun open-previous-line (arg)
  "Open a new line before the current one. 
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))


;; If the mark is active, kill the region, else kill the previous word

(defun back-kill-or-kill-region (arg)
  "Kill the region if active, else backwards kill a word"
  (interactive "p")
  (if (and transient-mark-mode mark-active)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))


;; For launching the default browser on os x

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

(provide 'utility)
