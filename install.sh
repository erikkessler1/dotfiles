#!/bin/sh

stow bash
stow emacs

# Link .bash_profile -> .bashrc
rm -f ~/.bash_profile
ln -s ~/.bashrc ~/.bash_profile

./install_python_packages.sh

