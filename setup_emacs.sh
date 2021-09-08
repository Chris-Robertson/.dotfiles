#!/bin/bash
set -e

# backup emacs config if it exists
# [ -f ~/.emacs ] && mv ~/.emacs ~/.emacs.bak
# [ -f ~/.emacs-profiles.el ] && mv ~/.emacs-profiles.el ~/.emacs-profiles.el.bak
# [ -d ~/.emacs.d ] && mv ~/.emacs.d ~/.emacs.d.bak
# [ -d ~/.spacemacs.d ] && mv ~/.spacemacs.d ~/.spacemacs.d.bak

# install chemacs
# git clone https://github.com/plexus/chemacs2.git ~/.emacs.d

# link chemacs config
rm ~/.emacs-profiles.el
ln -sv ~/.dotfiles/.emacs-profiles.el ~/.emacs-profiles.el

# link default config
# mkdir ~/.emacs-scratch.d
rm ~/.emacs-scratch.d/init.el
ln -sv ~/.dotfiles/init.el ~/.emacs-scratch.d/init.el

# link spacemacs config
# mkdir ~/.spacemacs.d
rm ~/.spacemacs.d/init.el
ln -sv ~/.dotfiles/spacemacs/init.el ~/.spacemacs.d/init.el

# install emacs on mac
## stable emacs is still 27 as of 2021-09-08, but we want 28
## native comp is a new feature in 28 that apparently increases performance
## https://www.reddit.com/r/emacs/comments/pj6m9m/which_emacs_should_i_use_for_macos/hbuh4sn/
brew tap d12frosted/emacs-plus
brew install emacs-plus@28 --with-native-comp
