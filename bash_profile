export PATH=$PATH:/Developer/usr/bin:/usr/local/sbin:/opt/local/bin:/opt/local/sbin:/usr/local/share/npm/bin:$HOME/.gitscripts
export CDPATH=$HOME/src

export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

export EDITOR="mate -w"

# for Go
export GOROOT=`brew --cellar`/go/HEAD
export GOBIN=/usr/local/bin
export GOARCH=amd64
export GOOS=darwin
export NODE_PATH=/usr/local/lib/node

function parse_git_branch {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo " ("${ref#refs/heads/}")"
}

BLACK="\[\e[1;30m\]"
RESET="\[\e[0m\]"
RED="\[\e[1;31m\]"

export PS1="$BLACK\h: \w$RED\$(parse_git_branch)$BLACK\$$RESET "

#export PYTHONPATH=/Library/Frameworks/Python.framework/Versions/2.6/bin/python

#export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/1.5/Home/

alias ls='ls -hG'
alias vim='/Applications/MacVim/mvim'
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'


#from http://theapotek.com/teknotes/2006/12/01/making-the-mac-os-x-bash-shell-alias-and-symlink-agnostic/
# teach shell to treat aliases like symbolic links rather than files
function cd {
	if [ ${#1} == 0 ]; then
		builtin cd
	elif [[ -d "${1}" || -L "${1}" ]]; then	# regular link or directory
		builtin cd "${1}"
	elif [ -f "${1}" ]; then	# file: is it an alias?
		# Redirect stderr to dev null to suppress OSA environment errors
		exec 6>&2 # Link file descriptor 6 with stderr so we can restore stderr later
		exec 2>/dev/null # stderr replaced by /dev/null
		path=$(osascript << EOF
tell application "Finder"
set theItem to (POSIX file "${1}") as alias
if the kind of theItem is "alias" then
get the posix path of ((original item of theItem) as text)
end if
end tell
EOF
)
		exec 2>&6 6>&-      # Restore stderr and close file descriptor #6.

		if [ "$path" == '' ];then # probably not an alias, but regular file
			builtin cd "${1}"	# will trigger regular shell error about cd to regular file
		else	# is alias, so use the returned path of the alias
			builtin cd "$path"
		fi
	else	# should never get here, but just in case.
		builtin cd "${1}"
	fi

}

if [[ -s /Users/matt/.rvm/scripts/rvm ]] ; then source /Users/matt/.rvm/scripts/rvm ; fi
