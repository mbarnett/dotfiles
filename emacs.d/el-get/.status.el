((ansi-color status "removed" recipe nil)
 (el-get status "installed" recipe
	 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :features el-get :info "." :load "el-get.el"))
 (eproject status "installed" recipe
	   (:name eproject :description "File grouping (\"project\") extension for emacs" :type github :pkgname "jrockway/eproject" :load-path
		  ("." "lang" "contrib")
		  :features eproject))
 (go-mode status "installed" recipe
	  (:name go-mode :description "Major mode for the Go programming language" :type http :url "http://go.googlecode.com/hg/misc/emacs/go-mode.el?r=tip" :localname "go-mode.el" :features go-mode :post-init
		 (add-to-list 'auto-mode-alist
			      '("\\.go\\'" . go-mode))))
 (nyan-mode status "installed" recipe
	    (:name nyan-mode :description "Nyan Cat for Emacs! Nyanyanyanyanyanyanyanyanyan!" :type github :pkgname "TeMPOraL/nyan-mode" :features nyan-mode))
 (sr-speedbar status "installed" recipe
	      (:name sr-speedbar :type emacswiki :description "Same frame speedbar" :post-init
		     (require 'sr-speedbar)))
 (tabbar status "installed" recipe
	 (:name tabbar :type emacswiki :description "Display a tab bar in the header line" :lazy t :load-path "."))
 (tabbar-ruler status "installed" recipe
	       (:name tabbar-ruler :website "https://github.com/mlf176f2/tabbar-ruler.el" :description "Tabbar ruler is an emacs package that allows both the tabbar and the ruler to be used together. In addition it allows auto-hiding of the menu-bar and tool-bar." :type github :depends tabbar :pkgname "mlf176f2/tabbar-ruler.el"))
 (yaml-mode status "installed" recipe
	    (:name yaml-mode :description "Simple major mode to edit YAML file for emacs" :type github :pkgname "yoshiki/yaml-mode" :prepare
		   (progn
		     (autoload 'yaml-mode "yaml-mode" nil t)
		     (add-to-list 'auto-mode-alist
				  '("\\.ya?ml\\'" . yaml-mode))))))
