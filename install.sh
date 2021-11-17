#!/bin/sh

stow bash
stow emacs
stow vscode -t "/Users/ekessler/Library/Application Support/Code/User/" --adopt

# Link .bash_profile -> .bashrc
rm -f ~/.bash_profile
ln -s ~/.bashrc ~/.bash_profile
