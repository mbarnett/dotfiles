;; Default frame size

(add-to-list 'default-frame-alist '(height . 52))
(add-to-list 'default-frame-alist '(width . 148))


;;; El-Get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  (package-refresh-contents)
  (package-initialize)
  (package-install 'el-get)
  (require 'el-get))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(setq el-get-emacswiki-base-url "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/")

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(require 'el-get-elpa)
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))
