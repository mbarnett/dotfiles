HISTFILE=~/.zhistfile
HISTSIZE=10000
SAVEHIST=10000
unsetopt beep
setopt SH_WORD_SPLIT

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

autoload -U select-word-style
select-word-style bash

autoload -U colors && colors

alias ls='ls -hG'
alias vim='/Applications/MacVim/mvim'
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
alias tmux='tmux -2'
alias pine='alpine'

export PATH=/usr/local/bin:$PATH:$HOME/.gitscripts:$HOME/.cabal/bin:$HOME/bin

export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

export EDITOR='emacs'

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

function parse_git_branch() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo " ("${ref#refs/heads/}")"
}

BOLD="%{$fg_bold[blue]%}"
RED="%{$fg[magenta]%}"
RESET="%{$reset_color%}"

function precmd() {
    PROMPT="$BOLD%m: %~$RESET$RED$(parse_git_branch)$BOLD%(!.#.$) $RESET"
}

function chpwd() {
    set_gopath_if_goproj
}


[[ -s "/Users/matt/.rvm/scripts/rvm" ]] && source "/Users/matt/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
