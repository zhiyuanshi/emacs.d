#!/bin/bash

cd ~/Dropbox/Dev/dotfiles

cp .vimrc ~     # dotfiles for Vim
cp .ocamlinit ~ # dotfiles for OCaml
cp .hgrc ~      # dotfiles for hg

# dotfiles for ocp-indent
# We don't remove the old directory, because it may contain other good things.
cp -r .ocp ~

rm -rf ~/.gconf/apps/gnome-terminal # Remove bash configuration

# Ammendments to .bashrc

cp /etc/skel/.bashrc ~
echo -e "\ncd ~/Dropbox/Dev" >> ~/.bashrc # Desired default directory
echo -e "bind \"set completion-ignore-case on\"\n" >> ~/.bashrc

# Set OPAM's environment variables upon each startup of bash
# https://realworldocaml.org/beta1/en/html/installation.html
echo -e "eval \`opam config -env\`\n" >> ~/.bashrc

# Re-definitions
echo 'function gvim () { (/usr/bin/gvim -f "$@" &) }'      >> ~/.bashrc
echo 'function vim  () { (/usr/bin/gvim -f "$@" &) }'      >> ~/.bashrc
echo 'function gitg () { (/usr/bin/gitg 2> /dev/null &) }' >> ~/.bashrc
