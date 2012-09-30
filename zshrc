HISTFILE=~/.zhistfile
HISTSIZE=10000
SAVEHIST=10000
unsetopt beep

# emacs style
bindkey -e

autoload -Uz compinit && compinit

autoload -U select-word-style
select-word-style bash

autoload -U colors && colors

export PATH=/usr/local/bin:$PATH:$HOME/.gitscripts:$HOME/.cabal/bin

export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

export EDITOR="emacs"

# Node.js
export NODE_PATH=/usr/local/lib/node

function parse_git_branch() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo " ("${ref#refs/heads/}")"
}

BOLD="%{$fg_bold[black]%}"
RED="%{$fg[red]%}"
RESET="%{$reset_color%}"

function precmd() {
    PROMPT="$BOLD%m: %~$RED$(parse_git_branch)$BOLD%(!.#.$) $RESET"
}

alias ls='ls -hG'
alias vim='/Applications/MacVim/mvim'
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
alias gdb='cgdb'
alias tmux="TERM=xterm-256color tmux"


[[ -s "/Users/matt/.rvm/scripts/rvm" ]] && source "/Users/matt/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
