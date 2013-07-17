#!/bin/bash

cd ~/Dropbox/Dev/dotfiles

# dotfiles for Vim
cp .vimrc ~
cp -r .vim ~

# dotfiles for OCaml
cp .ocamlinit ~

# dotfiles for ocp-indent
# We don't remove the old directory, because it may contain other good things.
cp -r .ocp ~

# dotfiles for hg
cp .hgrc ~

# Remove bash configuration
#rm -rf ~/.gconf/apps/gnome-terminal

# Ammendments to .bashrc

cp /etc/skel/.bashrc ~

echo -e "\nbind \"set completion-ignore-case on\"\n" >> ~/.bashrc

# Set OPAM's environment variables upon each startup of bash
# https://realworldocaml.org/beta1/en/html/installation.html
echo -e "eval \`opam config -env\`\n" >> ~/.bashrc

# Redefine gvim and emacs in bash
# Reserve the command 'vi' in case we want to use it in console mode
echo 'function gvim ()  { (/usr/bin/gvim -f "$@" &) }' >> ~/.bashrc
echo 'function vim  ()  { (/usr/bin/gvim -f "$@" &) }' >> ~/.bashrc
