#HISTFILE=~/.zhistfile
HISTSIZE=10000
SAVEHIST=10000
unsetopt beep
setopt SH_WORD_SPLIT

export DISABLE_PRY_RAILS=1

export PATH=$PATH:$HOME/bin

export DISABLE_SPRING=true
export HOMEBREW_NO_AUTO_UPDATE=1 
export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_INSTALL_CLEANUP=1

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
# alias ccl='ccl64'
# alias ccl32='/usr/local/bin/ccl'

export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad
export EDITOR='mg'

case "$OSTYPE" in
  darwin*)
    # export LDFLAGS="-L/usr/local/opt/postgresql@10/lib -L/usr/local/opt/mysql@5.6/lib"
    # export CPPFLAGS="-I/usr/local/opt/postgresql@10/include -I/usr/local/opt/mysql@5.6/include"
    # export PATH=$PATH::/usr/local/opt/postgresql@10/bin

    # openssl
    export PATH="/opt/homebrew/opt/openssl@1.1/bin:$PATH"
    export LDFLAGS="-L/opt/homebrew/opt/openssl@1.1/lib"
    export CPPFLAGS="-I/opt/homebrew/opt/openssl@1.1/include"
    export PKG_CONFIG_PATH="/opt/homebrew/opt/openssl@1.1/lib/pkgconfig"
    export RUBY_CONFIGURE_OPTS="--with-openssl-dir=/opt/homebrew/opt/openssl@1.1"
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

eval "$(/opt/homebrew/bin/brew shellenv)"

eval "$(rbenv init -)"

export NVM_DIR="$HOME/.nvm"
 [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
 [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion


export PATH="/opt/homebrew/opt/postgresql@16/bin:$PATH"
