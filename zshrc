HISTFILE=~/.zhistfile
HISTSIZE=10000
SAVEHIST=10000
unsetopt beep
setopt SH_WORD_SPLIT

export DISABLE_PRY_RAILS=1

export PATH=/usr/local/bin:/usr/local/sbin:$PATH:/sbin:$HOME/.cargo/bin:$HOME/bin:$HOME/.rbenv/bin

export DISABLE_SPRING=true

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
export EDITOR='mg'

case "$OSTYPE" in
  darwin*)
    export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@1.1)"
    export LDFLAGS="-L/usr/local/opt/postgresql@10/lib"
    export CPPFLAGS="-I/usr/local/opt/postgresql@10/include"
    export PATH=$PATH::/usr/local/opt/postgresql@10/bin
  ;;
  linux*)
    alias zyp='zypper'
  ;;
esac

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

eval "$(rbenv init -)"

#export NVM_DIR="$HOME/.nvm"
#[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

# place this after nvm initialization!
#autoload -U add-zsh-hook

#load-nvmrc() {
#  local node_version="$(nvm version)"
#  local nvmrc_path="$(nvm_find_nvmrc)"
#
#  if [ -n "$nvmrc_path" ]; then
#    local nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")
#
#    if [ "$nvmrc_node_version" = "N/A" ]; then
#      nvm install
#    elif [ "$nvmrc_node_version" != "$node_version" ]; then
#      nvm use
#    fi
#  elif [ "$node_version" != "$(nvm version default)" ]; then
#    echo "Reverting to nvm default version"
#    nvm use default
#}
#add-zsh-hook chpwd load-nvmrc
#load-nvmrc
