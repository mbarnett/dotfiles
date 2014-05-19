HISTFILE=~/.zhistfile
HISTSIZE=10000
SAVEHIST=10000
unsetopt beep
setopt SH_WORD_SPLIT


export MITRE_GEM_SERVER_USERNAME=mitremedia
export MITRE_GEM_SERVER_PASSWORD=d0notenter


case $TERM in
    screen*)
        ;; # tmux's TERM is set in .tmux.conf
    *)
        export TERM=xterm-256color
        ;;
esac

# emacs style
bindkey -e

autoload -Uz compinit && compinit
compdef -d git

autoload -U select-word-style
select-word-style bash

autoload -U colors && colors

alias ls='ls -hG'
alias vim='/Applications/MacVim/mvim'
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
alias tmux='tmux -2'
alias pine='alpine'
alias subl='/Applications/Sublime\ Text\ 2.app/Contents/SharedSupport/bin/subl'
alias ccl='/usr/local/bin/ccl64'
alias ccl32='/usr/local/bin/ccl'


export PATH=~/.rbenv/shims:/usr/local/bin:/usr/local/sbin:$PATH:/sbin:$HOME/.gitscripts:$HOME/.cabal/bin:$HOME/bin

export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

export EDITOR='mg'

# Node.js
export NODE_PATH=/usr/local/lib/node

# Go
export GOROOT=/usr/local/Cellar/go/1.0.3
export GOBIN=~/bin

function set_gopath_if_goproj() {
    unset GOPATH
    local cur=$PWD
    while [ $cur != '/' ]; do
        if [[ -a $cur/.goproj ]]
        then
            export GOPATH=$cur
            break
        else
            cur=$cur:h
        fi
    done
}

function parse_curr_git_branch_name() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo " ("${ref#refs/heads/}")"
}

BOLD="%{$fg_bold[blue]%}"
RED="%{$fg[magenta]%}"
RESET="%{$reset_color%}"

function precmd() {
    PROMPT="$BOLD%m: %~$RESET$RED$(parse_curr_git_branch_name)$BOLD%(!.#.$) $RESET"
}

function chpwd() {
    set_gopath_if_goproj
}
export CHEF_REPO=/Users/matt/Source/mitre/chef
export MITRE_COOKBOOKS=/Users/matt/Source/mitre/mitre-cookbooks

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
