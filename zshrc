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
alias tmux='tmux -2'
alias pine='alpine'

export PATH=~/.rbenv/shims:/usr/local/bin:/usr/local/sbin:$PATH:/sbin:$HOME/.gitscripts:$HOME/.cabal/bin:$HOME/bin:/usr/local/opt/go/libexec/bin:/opt/p4merge/bin

export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

export EDITOR='mg'

# Go
#export GOROOT=/usr/local/Cellar/go/1.0.3
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

function git_stash_count() {
    count=$(git stash list 2> /dev/null | wc -l) || return
    if [[ $count -gt 0 ]]
    then
      echo "{*${count}}"
    fi
}

BOLD="%{$fg_bold[blue]%}"
RED="%{$fg[magenta]%}"
ALERT="%{$fg[yellow]%}"
RESET="%{$reset_color%}"

function precmd() {
    PROMPT="$BOLD%m: %~$RESET$RED$(parse_curr_git_branch_name)$BOLD$ALERT$(git_stash_count)$RESET$BOLD%(!.#.$) $RESET"
}

function chpwd() {
    set_gopath_if_goproj
}

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
