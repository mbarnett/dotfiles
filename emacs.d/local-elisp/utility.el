;; Utility functions

(setq fallback-font "Courier-12")

; Portably specify default fonts like Apple Menlo

(defun set-font-if-exists (font)
  (if (not (eq window-system nil))
      (if (null (x-list-fonts font))
          (set-default-font fallback-font) (set-default-font font))
    nil))


; A quieter bell function

(defun quieter-bell ()
  (unless (memq this-command
    	'(isearch-abort abort-recursive-edit exit-minibuffer
              keyboard-quit mwheel-scroll down up next-line previous-line
              backward-char forward-char))
    (ding)))


; Set the backup dir more cleanly

(defun set-backup-dir (dir)
  (setq backup-directory-alist
        `(( "." . ,dir))))

(provide 'utility)