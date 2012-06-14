export PATH=/usr/local/bin:$PATH:/Developer/usr/bin:/usr/local/sbin:/usr/local/share/npm/bin:$HOME/.gitscripts:$HOME/.cabal/bin:$HOME/Tools/depot_tools

export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

export EDITOR="emacs"

export PYTHONPATH=/usr/local/lib/python2.7/site-packages/

# Go
# export GOROOT=`brew --cellar`/go/HEAD
# export GOBIN=/usr/local/bin
# export GOARCH=amd64
# export GOOS=darwin

# Node.js
export NODE_PATH=/usr/local/lib/node

function parse_git_branch {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo " ("${ref#refs/heads/}")"
}

BLACK="\[\e[1;30m\]"
RESET="\[\e[0m\]"
RED="\[\e[1;31m\]"

export PS1="$BLACK\h: \w$RED\$(parse_git_branch)$BLACK\$$RESET "


alias ls='ls -hG'
alias vim='/Applications/MacVim/mvim'
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
alias gdb='cgdb'
alias tmux="TERM=xterm-256color tmux"


[[ -s "/Users/matt/.rvm/scripts/rvm" ]] && source "/Users/matt/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
