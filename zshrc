HISTFILE=~/.zhistfile
HISTSIZE=10000
SAVEHIST=10000
unsetopt beep
setopt SH_WORD_SPLIT
export DISABLE_SPRING=1
export DISABLE_PRY_RAILS=1
# Seriously, Homebrew?
export HOMEBREW_NO_ANALYTICS=1

export PATH=/usr/local/bin:/usr/local/sbin:$PATH:/sbin:$HOME/bin

case $TERM in
    screen*)
        ;; # tmux's TERM is set in .tmux.conf
    *)
        export TERM=xterm-256color
        ;;
esac

ulimit -n 2048

# emacs style
bindkey -e

autoload -Uz compinit && compinit

autoload -U select-word-style
select-word-style bash

autoload -U colors && colors

alias ls='ls -hG'
alias tmux='tmux -2'
alias pine='alpine'
alias ccl='ccl64'
alias ccl32='/usr/local/bin/ccl'

export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad
export DISABLE_PRY_RAILS=1
export EDITOR='mg'

function parse_curr_git_branch_name() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo " ("${ref#refs/heads/}")"
}

function git_stash_count() {
    count=$(git stash list 2> /dev/null | wc -l | awk '{print $1}') || return
    if [[ $count -gt 0 ]]
    then
      echo "(⚡️ ${count})"
    fi
}

function jobs_count() {
    count=$((jobs -s) | wc -l | awk '{print $1}') || return
    if [[ $count -gt 0 ]]
    then
        echo "(♺ ${count})"
    fi
}

BOLD="%{$fg_bold[blue]%}"
RED="%{$fg[magenta]%}"
ALERT="%{$fg[yellow]%}"
RESET="%{$reset_color%}"

function precmd() {
    PROMPT="$BOLD%m: %~$RESET$RED$(parse_curr_git_branch_name)$BOLD$ALERT$(git_stash_count)$RESET$BOLD$ALERT$(jobs_count)$RESET$BOLD%(!.#.✨) $RESET "
}

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
source ~/.cargo/env
[[ -s /Users/matt/.rvm/scripts/rvm ]] && source /Users/matt/.rvm/scripts/rvm # Load RVM into a shell session *as a function*
