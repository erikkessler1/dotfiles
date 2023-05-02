# My Dotfiles

> Configuration for various tools.

## Installation

Use GNU Stow to create symlinks to the home directory, `install.sh`

### Emacs 27.0

```bash
$ brew tap d12frosted/emacs-plus
$ brew install emacs-plus@28 --with-no-titlebar --with-nobu417-big-sur-icon
```

### Rubies

```bash
$ ruby-install ruby 3.2.2 && source ~/.bash_profile && chruby 3.2.2 && gem install 'rotp'
```
