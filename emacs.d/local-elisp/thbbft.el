;;; ack-thbbft.el --- A natural companion for ack and eproject

;; Copyright (C) 2012

;; Author: Matt Barnett <matt@sixtyodd.com>
;; Version: 0.1
;; Keywords: ack thbbft eproject bill-the-cat

;; This file is not a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.
;;
;;; Code:

;; (require 'compile)
;; (require 'grep)
;; (require 'thingatpt)

;; (define-compilation-mode thbbft-mode "Ack"
;;   "Ack results compilation mode."
;;   (set (make-local-variable 'truncate-lines) t)
;;   (set (make-local-variable 'compilation-disable-input) t)
;;   (let ((smbl  'compilation-ack-nogroup)
;;         (pttrn '("^\\([^:\n]+?\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3)))
;;     (set (make-local-variable 'compilation-error-regexp-alist) (list smbl))
;;     (set (make-local-variable 'compilation-error-regexp-alist-alist) (list (cons smbl pttrn))))
;;   (set (make-local-variable 'compilation-error-face) grep-hit-face))

;; ;; (setq thbbft-executable (executable-find "ack"))

;; ;; (defvar ack-history nil
;; ;;   "History for the `ack' command.")

;; ;; (defun thbbft (command-args)
;; ;;   (interactive
;; ;;    (let ((ack-command-args "--nogroup --with-filename --all "))
;; ;;      (list (read-shell-command "Run ack (like this): "
;; ;;                                thbbft-executable
;; ;;                                ack-command-args
;; ;;                                'ack-history))))
;; ;;   (let ((compilation-disable-input t))
;; ;;     (compilation-start (concat command-args " < " null-device)
;; ;;                        'thbbft-mode)))

;; (require 'compile)
;; (require 'thingatpt)

;; (defvar thbbft-executable "ack" "The command run by the ack function.")

;; (defvar ack-mode-font-lock-keywords
;;   '(("^\\(Compilation\\|Ack\\) started.*"
;;      (0 '(face nil message nil help-echo nil mouse-face nil) t))))

;; (defvar ack-use-search-in-buffer-name t
;;   "If non-nil, use the search string in the ack buffer's name.")

;; ;; (define-compilation-mode ack-mode "Ack"
;; ;;   "Specialization of compilation-mode for use with ack."
;; ;;   nil)

;; (defun thbbft (dir pattern args)
;;   "Run ack, with user-specified ARGS, and collect output in a buffer.
;; While ack runs asynchronously, you can use the \\[next-error] command to
;; find the text that ack hits refer to. The command actually run is
;; defined by the ack-command variable."
;;   (interactive (list (read-file-name "Run ack in directory: " nil "" t)
;;                      (read-string "Search for: " (thing-at-point 'symbol))
;;                      (read-string "Ack arguments: " "-i" nil "-i" nil)
;;                      ))
;;                                         ; Get dir into an the right state, incase a file name was used
;;   (setq dir (abbreviate-file-name
;;              (file-name-as-directory (expand-file-name dir))))
;;   ;; Check that it's really a directory.
;;   (or (file-directory-p dir)
;;       (error "ack needs a directory: %s" dir))

;;   (let (compile-command
;;         (compilation-error-regexp-alist grep-regexp-alist)
;;         (compilation-directory default-directory)
;;         (ack-full-buffer-name (concat "ack-" pattern)))
;;     ;; (save-some-buffers (not compilation-ask-about-save) nil)
;;     ;; lambda defined here since compilation-start expects to call a function to get the buffer name
    ;; (compilation-start (concat thbbft-executable " " args " " pattern " " dir) 'thbbft-mode
    ;;                    (when ack-use-search-in-buffer-name
    ;;                      (function (lambda (ignore)
    ;;                                  ack-full-buffer-name)))
    ;;                    (regexp-quote pattern))))


(require 'compile)
(require 'grep)
(require 'eproject)
(require 'thingatpt)

(defvar thbbft-executable (executable-find "ack")
  "The full path of the ack executable for thbbft to run")

;; (defvar ack-guess-type nil
;;   "Setting this value to `t' will have `ack' do its best to fill
;; in the --type argument to the ack command")

(defvar thbbft-options "-A 1 -B 2"
  "Options to be passed to ack")

(defvar thbbft-mandatory-options "--nocolor --nogroup --column --sort-files"
  "Options necessary for thbbft-ack to work")
(make-variable-buffer-local 'thbbft-mandatory-options)

;; (defvar ack-mode-type-map
;;   '(((c++-mode) . "cpp")
;;     ((c-mode) . "cc")
;;     ((css-mode) . "css")
;;     ((emacs-lisp-mode) . "elisp")
;;     ((fortran-mode) . "fortran")
;;     ((html-mode) . "html")
;;     ((xml-mode nxml-mode) . "xml")
;;     ((java-mode) . "java")
;;     ((lisp-mode) . "lisp")
;;     ((perl-mode cperl-mode) . "perl"))
;;   "alist describing how to fill in the '--type=' argument to ack")

;; (defun ack-find-type-for-mode ()
;;   (catch 'found
;;     (dolist (mode-type ack-mode-type-map)
;;       (when (member major-mode (car mode-type))
;;         (throw 'found (cdr mode-type))))))


(define-compilation-mode thbbft-mode "Thbbft"
  "thbfft-ack compilation mode."
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-error-face)
       grep-hit-face))

(defun thbbft-find-or-request-project-root ()
  (condition-case nil
      (eproject-root)
    (error (read-file-name "Run ack in directory: " nil "" t))))

(defun thbbft-find-or-request-ack ()
  (if thbbft-executable
      thbbft-executable
    (read-string "Ack executable: " nil)))


;;;###autoload
(defun thbbft (ack pattern target)
  (interactive (list (thbbft-find-or-request-ack)
                      (read-string "Search for: " (thing-at-point 'symbol))
                      (thbbft-find-or-request-project-root)))
  (let ((full-command (concat ack " " thbbft-options " " thbbft-mandatory-options " " pattern " " target)))
   (compilation-start full-command 'thbbft-mode)))

(defun my-compilation-hook ()
  (when (not (get-buffer-window "*thbbft*"))
    (save-selected-window
      (save-excursion
        (pop-to-buffer-same-window "*thbbft*")))))

(add-hook 'compilation-mode-hook 'my-compilation-hook)

(provide 'thbbft)
