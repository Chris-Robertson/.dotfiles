#!/bin/bash

# backup emacs config if it exists
[ -f ~/.emacs ] && mv ~/.emacs ~/.emacs.bak
[ -d ~/.emacs.d ] && mv ~/.emacs.d ~/.emacs.default

# install chemacs
git clone https://github.com/plexus/chemacs2.git ~/.emacs.d

# link chemacs config
ln -sv ~/.dotfiles/.emacs-profiles.el ~/.emacs-profiles.el


