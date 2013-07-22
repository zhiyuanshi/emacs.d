#!/bin/bash

cd ~/Dropbox/Dev/dotfiles

# Home directory
cp .vimrc ~     # dotfiles for Vim
cp .ocamlinit ~ # dotfiles for OCaml
cp .hgrc ~      # dotfiles for hg
cp -r .ocp ~    # We don't remove the old directory, because it may contain other good things.
rm -rf ~/.gconf/apps/gnome-terminal # Remove bash configuration

# Repos
for dir in $(ls ..)
do
  if [ "$dir" != "dotfiles" ] && [ -d "../$dir" ]; then
    cp .gitignore "../$dir"
    if [ $(ack-grep -f "../$dir" -G '\.ml$' -1 | wc -l) -ne "0" ]; then
      echo "OCaml: $dir"
      cp _tags "../$dir"
    fi
  fi
done

# .bashrc

cp /etc/skel/.bashrc ~
echo -e "\ncd ~/Dropbox/Dev" >> ~/.bashrc # Desired default directory
echo -e "bind 'set completion-ignore-case on'\n" >> ~/.bashrc

# Set OPAM's environment variables upon each startup of bash
# https://realworldocaml.org/beta1/en/html/installation.html
echo -e "eval \`opam config -env\`\n" >> ~/.bashrc

echo "alias ack='ack-grep'" >> ~/.bashrc
echo 'function gvim () { (/usr/bin/gvim "$@" 2> /dev/null &) }' >> ~/.bashrc
echo 'function vim  () { (/usr/bin/gvim "$@" 2> /dev/null &) }' >> ~/.bashrc
echo 'function gitg () { (/usr/bin/gitg "$@" 2> /dev/null &) }' >> ~/.bashrc
