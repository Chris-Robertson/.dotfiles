#!/bin/bash

# backup emacs config if it exists
[ -f ~/.emacs ] && mv ~/.emacs ~/.emacs.bak
[ -f ~/.emacs-profiles.el ] && mv ~/.emacs-profiles.el ~/.emacs-profiles.el.bak
[ -d ~/.emacs.d ] && mv ~/.emacs.d ~/.emacs.d.bak
[ -d ~/.spacemacs.d ] && mv ~/.spacemacs.d ~/.spacemacs.d.bak

# install chemacs
git clone https://github.com/plexus/chemacs2.git ~/.emacs.d

# link chemacs config
ln -sv ~/.dotfiles/.emacs-profiles.el ~/.emacs-profiles.el

# link spacemacs config
mkdir ~/.spacemacs.d
ln -sv ~/.dotfiles/spacemacs/init.el ~/.spacemacs.d/init.el

