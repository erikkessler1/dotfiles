# My Dotfiles

> Configuration for various tools.

## Installation

First, add a GitHub SSH key:

    read -p "Enter email for SSH Key: " email && ssh-keygen -t ed25519 -C "$email" && eval "$(ssh-agent -s)" && echo -e "Host github.com\n  AddKeysToAgent yes\n  IdentityFile ~/.ssh/id_ed25519" > ~/.ssh/config && ssh-add --apple-use-keychain ~/.ssh/id_ed25519 && pbcopy < ~/.ssh/id_ed25519.pub && open https://github.com/settings/keys

Then clone this repository:

    git clone git@github.com:erikkessler1/dotfiles.git

Once the respository is cloned, run:

     cd dotfiles
    ./install.sh

## Manual Settings

These are some things to manually set in System Preferences:

- Trackpad > Point & Click > Tracking Speed: Fast - 1
- Trackpad > Scroll & Zoom > Natural Scrolling: Off
- Keyboard > Key repeat rate: Fast
- Keyboard > Delay until repeat: Short
- Keyboard > Keyboard Shortcuts > Modifier Keys > Caps Lock: Control
- Desktop & Dock > Dock > Automatically hide and show the Dock: On
- Desktop & Dock > Windows > Tiled windows have margins > Off
- Users & Groups > *command-click* > Advanced Options > Login shell: /bin/bash

In Emacs:

- Run `nerd-icons-install-fonts` and restart Emacs

In iTerm2:

- Settings > Settings > Load settings from a custom folder
  - Set to: dotfiles/iterm2 and "Automatically"
  - Close iTerm
  - Revert any changes
  - Open iTerm
