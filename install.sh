#!/usr/bin/env bash

# remove pre-existing files and create symlinks to my ones
rm -f ~/.alias
ln -sv ~/.dotfiles/.alias ~/.alias

rm -f ~/.bashrc
ln -sv ~/.dotfiles/.bashrc ~/.bashrc

rm -f ~/.gitignore_global
ln -sv ~/.dotfiles/.gitignore_global ~/.gitignore_global

rm -f ~/.ideavimrc
ln -sv ~/.dotfiles/.ideavimrc ~/.ideavimrc

rm -f ~/.tmux.conf
ln -sv ~/.dotfiles/.tmux.conf ~/.tmux.conf

rm -f ~/.vimrc
ln -sv ~/.dotfiles/.vimrc ~/.vimrc

rm -f ~/.zshrc
ln -sv ~/.dotfiles/.zshrc ~/.zshrc
