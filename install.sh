#!/bin/sh

if ! [[ $(command -v brew) ]]; then
    echo 'Installing Homebrew...'
    /bin/bash -c \
        "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)" \
        || echo 'Error installing Homebrew'
    eval "$(/opt/homebrew/bin/brew shellenv)"
    brew update
else
    echo 'Homebrew already installed'
fi

brew bundle --file=Brewfile

stow bash
stow emacs
stow vscode -t "$HOME/Library/Application Support/Code/User/" --adopt
ln -sf "$PWD/.prettierrc.yaml" ~/.prettierrc.yaml

# Link .bash_profile -> .bashrc
rm -f ~/.bash_profile
ln -s ~/.bashrc ~/.bash_profile
source ~/.bash_profile

cp "$PWD/bash/.bashrc.d/secrets.bash.example" "$PWD/bash/.bashrc.d/secrets.bash"

read -p "Enter email for git: " email
git config --global user.name "Erik Kessler"
git config --global user.email $email

# Node packages
volta install node
npm install -g cspell
npm install -g prettier

# Rubies
if ! [[ $(command -v rustc) ]]; then
    echo 'Installing rustc...'
    echo 'Change options so that .bash_profile *not* modified!'
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
fi
add-ruby 3.3.6

# Emacs
osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/Cellar/emacs-plus\@29/29.4/Emacs.app" at posix file "/Applications" with properties {name:"Emacs.app"}'
