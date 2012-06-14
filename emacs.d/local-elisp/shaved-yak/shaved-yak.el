;;; shaved-yak.el --- Emacs binding to the `fuzzy_file_finder' rubygem.

;; Author: Justin Weiss
;; URL: http://github.com/avvo/shaved-yak/tree/master
;; Version: 1.0
;; Created: 2008-10-14
;; Keywords: project, convenience

;; This file is NOT part of GNU Emacs.

;;; License:

;; Copyright (c) 2008 Justin Weiss
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Requires ruby and ansi-color.el
;;
;; The `fuzzy_file_finder' rubygem can be installed with the following command:
;; `sudo gem install --source http://gems.github.com jamis-fuzzy_file_finder'

;;; Usage:
;; The primary interface into the functionality provided by this file is through
;; the `shaved-yak' function. Calling this function will match the query to
;; the files under `fuzzy-find-project-root' and open up a completion buffer with
;; the first matched file selected (with a `> '.) The selection can be changed using
;; `C-n' and `C-p', and the currently selected file can be opened using `<RET>'.

;;; Configuration:
;; In your .emacs or init.el:
;;
;; (add-to-list 'load-path "~/.emacs.d/path/to/shaved-yak")
;; (require 'shaved-yak)
;; (fuzzy-find-project-root "~/path/to/project")

;;; TODO:
;; - Clean up *Completions* buffer on exit
;; - Use project-local-variables to scope the find to the current file's project
;; - misc. cleanup and error handling (make sure process is killed on failure, etc.)

;;; Code:

(require 'ansi-color)
(require 'eproject)

(defvar shaved-yak-project-root "."
  "The root directory in which to recursively look for files")

(defvar shaved-yak-initialized nil
  "Tracks whether or not the fuzzy finder has been initialized.")

(defvar shaved-yak-completion-buffer-name "*Completions*"
  "The name of the buffer to display the possible file name completions")

(defvar shaved-yak-mode nil
  "Tells the minibuffer when to use the fuzzy finder")

(defvar shaved-yak-process nil
  "Holds the process that runs the fuzzy_find_file rubygem")

(defvar shaved-yak-completions ""
  "Contains the current file name completions")

(defvar shaved-yak-selected-completion-index 1
  "1-based index of the currently selected completion")

(defvar shaved-yak-setup-hook nil
  "Hook that runs after shaved-yak initialization")

(defvar shaved-yak-search-cmd "{
  search = $0
  gsub(/ /, \"\", search)
    
  split(search, a, \"\")
  len = length(search)
  sregex = \"/\"
  
  for(i = 1; i <= len; i++) {
    sregex = sregex \"([\" tolower(a[i]) toupper(a[i]) \"])(.*)\"
  }
  sregex = sregex \"/\"

  system(\"find /Users/matt/Source/wtm/opal -type f | grep -vE \'.git\' | sed \'s:/Users/matt/Source/wtm/opal/::\'  | awk \'match($0, \" sregex \") { print RLENGTH \\\"\\t\\\" $0 }\' | sort -k1n | cut -f2\")

  print \"DONE\"
}")

(defun shaved-yak-find-file (process query)
  "Communicates with the ruby process, sending the query string `query' and retrieves the possible completions as a string."
  (setq shaved-yak-completions "")
  (process-send-string process (concat query "\n"))
   (let ((count 0))
     (while (and (not (string-suffix-p "\nDONE\n" shaved-yak-completions)) (> 20 count))
       (sleep-for 0 1)
       (setq count (1+ count))))
  (setq shaved-yak-completions (string-trim-end shaved-yak-completions (length "\DONE\n")))
  shaved-yak-completions)

(defun string-trim-end (string num-chars)
  "Trims `num-chars' from the end of `string'. Returns the empty string if `num-chars' is larger than the length of `string'."
  (if (< num-chars (length string))
    (substring string 0 (- (length string) num-chars))
    ""))

(defun string-suffix-p (suffix string)
  "Determines whether the string `string' ends with the suffix `suffix'."
  (if (> (length suffix) (length string))
    nil
    (eq t (compare-strings string (- (length string) (length suffix)) nil suffix 0 nil))))

(defun shaved-yak-get-completions (process output)
  "The process filter for retrieving data from the fuzzy_file_finder ruby gem"
  (setq shaved-yak-completions (concat shaved-yak-completions output)))

(defun shaved-yak-command-hook ()
  "A hook to fuzzy find whatever string is in the minibuffer following the prompt.
Displays the completion list along with a selector in the `*Completions*' buffer.
The hook runs on each command."
  (unless (eq (process-status shaved-yak-process) 'exit)
    (setq shaved-yak-selected-completion-index 1)
    (let ((query-string (buffer-substring-no-properties (minibuffer-prompt-end) (point-max))))
      (get-buffer-create shaved-yak-completion-buffer-name)
      (set-buffer shaved-yak-completion-buffer-name)
      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert (shaved-yak-find-file shaved-yak-process query-string))
        (shaved-yak-mark-line shaved-yak-selected-completion-index))
      (goto-char (point-min)) 
      (forward-line (1- shaved-yak-selected-completion-index))
      (ansi-color-apply-on-region (point-min) (point-max))
      (display-buffer shaved-yak-completion-buffer-name))))

;;;###autoload
(defun shaved-yak-goto-file ()
  "The main function for finding a file in a project.

This function opens a window showing possible completions for the letters typed into the minibuffer. By default the letters complete the file name; however, the finder can also complete on paths by typing a `/' into the minibuffer after the letters making up a path component. Move between selections using `C-n' and `C-p', and select a file to open using `<RET>'."
  (interactive)
  (setq shaved-yak-mode t)
  (shaved-yak-initialize)
  (setq shaved-yak-project-root (eproject-root))
  (when (null shaved-yak-project-root)
    (error
     (concat
      "Can't find a suitable project root")))
  (run-hooks 'shaved-yak-setup-hook)
  (format shaved-yak-search-cmd shaved-yak-project-root shaved-yak-project-root "")
  (setq shaved-yak-process (start-process "ffip" nil "/usr/bin/awk" shaved-yak-search-cmd))
  (add-hook 'minibuffer-setup-hook 'shaved-yak-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook 'shaved-yak-minibuffer-exit)
  (set-process-filter shaved-yak-process 'shaved-yak-get-completions)
  (read-string "Find file: ")
  (cond
   ((eq shaved-yak-exit 'find-file)
    (set-buffer shaved-yak-completion-buffer-name)
    (let ((buffer-read-only nil))
      (shaved-yak-unmark-line shaved-yak-selected-completion-index))
    (find-file (shaved-yak-read-line shaved-yak-selected-completion-index)))))

(defun shaved-yak-minibuffer-setup ()
  "Setup hook for the minibuffer"
  (when (eq shaved-yak-mode t)
    (add-hook 'post-command-hook 'shaved-yak-command-hook nil t)
    (use-local-map shaved-yak-keymap)))

(defun shaved-yak-minibuffer-exit ()
  "Cleanup code when exiting the minibuffer"
  (when (eq shaved-yak-mode t)
    (when (eq (process-status shaved-yak-process) 'run)
      (interrupt-process shaved-yak-process))
    (use-local-map (keymap-parent shaved-yak-keymap))
    (setq shaved-yak-mode nil)))

(defun shaved-yak-initialize ()
  "Initialize the keymap and other things that need to be setup before the first run of the fuzzy file finder."
  (if (not shaved-yak-initialized)
      (progn
        (setq shaved-yak-keymap (make-sparse-keymap))
        (set-keymap-parent shaved-yak-keymap minibuffer-local-map)
        (define-key shaved-yak-keymap "\C-n" 'shaved-yak-next-completion)
        (define-key shaved-yak-keymap "\C-p" 'shaved-yak-previous-completion)
        (define-key shaved-yak-keymap (kbd "<down>") 'shaved-yak-next-completion)
        (define-key shaved-yak-keymap (kbd "<up>") 'shaved-yak-previous-completion)
        (define-key shaved-yak-keymap "\r" 'shaved-yak-select-completion)
        (setq shaved-yak-initialized t))))

;;unwind-protect around main loop?

(defun shaved-yak-read-line (line-number)
  "Reads line `line-number' from the current buffer."
  (save-excursion
    (goto-char (point-min)) (forward-line (1- shaved-yak-selected-completion-index))
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun shaved-yak-select-completion ()
  "Selects the file at location `shaved-yak-completion-index' and exits the minibuffer."
  (interactive)
  (setq shaved-yak-exit 'find-file)
  (exit-minibuffer))

(defun shaved-yak-mark-line (line-number)
  "Highlights line at `line-number' and inserts '> ' at the beginning of line"
  (save-excursion
    (goto-char (point-min)) (forward-line (1- shaved-yak-selected-completion-index))
    (insert "> ")
    (add-text-properties (line-beginning-position) (line-end-position) '(face highlight))))

(defun shaved-yak-unmark-line (line-number)
  "Removes '> ' from the beginning of line `line-number' if it begins with '> '."
  (save-excursion
    (goto-char (point-min)) (forward-line (1- shaved-yak-selected-completion-index))
      (if (string-prefix-p "> " (shaved-yak-read-line line-number))
          (progn
            (delete-char 2)
            (remove-text-properties (line-beginning-position) (line-end-position) '(face nil))))))

(defun shaved-yak-mark-completion (completion-index-delta)
  "Moves the completion index marker by `completion-index-delta' and marks the line corresponding to the currently selected completion."
  (set-buffer shaved-yak-completion-buffer-name)
  (let ((buffer-read-only nil))
    (shaved-yak-unmark-line shaved-yak-selected-completion-index)
    (setq shaved-yak-selected-completion-index (+ completion-index-delta shaved-yak-selected-completion-index))
    ;; reset completion index if it falls out of bounds
    (if (< shaved-yak-selected-completion-index 1) (setq shaved-yak-selected-completion-index 1))
    (if (> shaved-yak-selected-completion-index (count-lines (point-min) (point-max))) (setq shaved-yak-selected-completion-index (count-lines (point-min) (point-max))))
    (shaved-yak-mark-line shaved-yak-selected-completion-index))
    (goto-char (point-min)) (forward-line (1- shaved-yak-selected-completion-index))
    ;; make sure the window scrolls correctly
    (set-window-point (get-buffer-window shaved-yak-completion-buffer-name) (point)))

(defun shaved-yak-next-completion ()
  "Selects the next completion."
  (interactive)
  (shaved-yak-mark-completion 1))

(defun shaved-yak-previous-completion ()
  "Selects the previous completion."
  (interactive)
  (shaved-yak-mark-completion -1))

(provide 'shaved-yak)
