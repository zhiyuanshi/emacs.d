#!/usr/bin/env bash

set -e
set -x

# Remove existing configuration and packages:
rm -rf ~/.vimrc ~/.vim

# Set up Vundle:
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

# Restore dotfiles:
ruby "$DOTFILES/copy_dotfiles.rb"

# Install plugins:
gvim +PluginInstall +qall

# Build YouCompleteMe:
cd ~/.vim/bundle/YouCompleteMe && ./install.sh --clang-completer

# Build vimproc.vim:
cd ~/.vim/bundle/vimproc.vim && make