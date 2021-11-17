#brew
eval "$(/opt/homebrew/bin/brew shellenv)"

# iterm
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

if [ $ITERM_SESSION_ID ]; then
  export PROMPT_COMMAND='echo -ne "\033];${PWD##*/}\007"; '
fi

# fzf
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# chruby
source $(brew --prefix)/opt/chruby/share/chruby/chruby.sh
chruby "2.7.2"

# git-completion
source $HOME/bin/git-completion.sh
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

# nvm
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
