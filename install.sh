#!/bin/sh

if ! [[ $(command -v brew) ]]; then
    echo 'Installing Homebrew...'
    /bin/bash -c \
        "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)" \
        || echo 'Error installing Homebrew'
    brew update
else
    echo 'Homebrew already installed'
fi

brew bundle --file=Brewfile

stow bash
stow emacs
stow vscode -t "$HOME/Library/Application Support/Code/User/" --adopt
ln -s "$PWD/.prettierrc.yaml" ~/.prettierrc.yaml

# Link .bash_profile -> .bashrc
rm -f ~/.bash_profile
ln -s ~/.bashrc ~/.bash_profile
source ~/.bash_profile

read -p "Enter email for git: " email
git config --global user.name "Erik Kessler"
git config --global user.email $email

# Node packages
volta install node
npm install -g cspell
npm install -g prettier

# Rubies
add-ruby 3.3.6
