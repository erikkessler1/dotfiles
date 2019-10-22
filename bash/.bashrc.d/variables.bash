source $HOME/.bin/git-completion.sh
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

export NVM_DIR="$HOME/.nvm"
export KAFKA_DIR="/Users/ekessler/kafka"

[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"

source /usr/local/share/chruby/chruby.sh
chruby "2.6.5"

