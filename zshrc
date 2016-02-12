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

export PATH=/usr/local/bin:/usr/local/sbin:$PATH:/sbin:$HOME/bin

export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

export EDITOR='mg'

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

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
