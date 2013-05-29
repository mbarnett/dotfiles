((ace-jump-mode status "installed" recipe
                (:name ace-jump-mode :website "https://github.com/winterTTr/ace-jump-mode/wiki" :description "A quick cursor location minor mode for emacs" :type github :pkgname "winterTTr/ace-jump-mode" :features ace-jump-mode))
 (ack status "removed" recipe nil)
 (ansi-color status "removed" recipe nil)
 (el-get status "installed" recipe
         (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :features el-get :info "." :load "el-get.el"))
 (eproject status "installed" recipe
           (:name eproject :description "File grouping (\"project\") extension for emacs" :type github :pkgname "jrockway/eproject" :load-path
                  ("." "lang" "contrib")
                  :features eproject))
 (full-ack status "installed" recipe
           (:name full-ack :description "A front-end for ack" :type github :pkgname "nschum/full-ack" :prepare
                  (progn
                    (autoload 'ack "full-ack" nil t)
                    (autoload 'ack-find-file "full-ack" nil t)
                    (autoload 'ack-find-same-file "full-ack" nil t)
                    (autoload 'ack-same "full-ack" nil t))))
 (go-mode status "installed" recipe
          (:name go-mode :description "Major mode for the Go programming language" :type http :url "http://go.googlecode.com/hg/misc/emacs/go-mode.el?r=tip" :localname "go-mode.el" :features go-mode :post-init
                 (add-to-list 'auto-mode-alist
                              '("\\.go\\'" . go-mode))))
 (highlight-indentation status "removed" recipe nil)
 (linum-off status "installed" recipe
            (:name linum-off :description "Provides an interface for turning line-numbering off" :type emacswiki :features linum-off))
 (nyan-mode status "installed" recipe
            (:name nyan-mode :description "Nyan Cat for Emacs! Nyanyanyanyanyanyanyanyanyan!" :type github :pkgname "TeMPOraL/nyan-mode" :features nyan-mode))
 (paredit status "removed" recipe nil)
 (rainbow-delimiters status "installed" recipe
                     (:name rainbow-delimiters :website "https://github.com/jlr/rainbow-delimiters#readme" :description "Color nested parentheses, brackets, and braces according to their depth." :type github :pkgname "jlr/rainbow-delimiters" :features rainbow-delimiters))
 (smart-tab status "removed" recipe nil)
 (smarttabs status "removed" recipe nil)
 (smooth-scroll status "installed" recipe
                (:name smooth-scroll :description "Minor mode for smooth scrolling." :type emacswiki :features smooth-scroll))
 (sr-speedbar status "installed" recipe
              (:name sr-speedbar :type emacswiki :description "Same frame speedbar" :post-init
                     (require 'sr-speedbar)))
 (tabbar status "installed" recipe
         (:name tabbar :type emacswiki :description "Display a tab bar in the header line" :lazy t :load-path "."))
 (tabbar-ruler status "removed" recipe nil)
 (vline status "installed" recipe
        (:name vline :description "show vertical line (column highlighting) mode." :type emacswiki :features vline))
 (yaml-mode status "installed" recipe
            (:name yaml-mode :description "Simple major mode to edit YAML file for emacs" :type github :pkgname "yoshiki/yaml-mode" :prepare
                   (progn
                     (autoload 'yaml-mode "yaml-mode" nil t)
                     (add-to-list 'auto-mode-alist
                                  '("\\.ya?ml\\'" . yaml-mode))))))
