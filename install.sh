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

# Link .bash_profile -> .bashrc
rm -f ~/.bash_profile
ln -s ~/.bashrc ~/.bash_profile

read -p "Enter email for git: " email
git config --global user.name "Erik Kessler"
git config --global user.email $email
