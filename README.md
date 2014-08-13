# zhiyuanshi dotfiles

## Prerequisites

Set zsh as your login shell:

    chsh -s $(which zsh)

Set variables:

    export DOTFILES=~/Dropbox/Code/dotfiles

## Reset Vim

Remove existing dotfiles:

    rm -rf ~/.vimrc ~/.vim

Install [Vundle](https://github.com/gmarik/Vundle.vim).

Restore dotfiles:

    ruby copy-dotfiles.rb

Open Vim and run `:PluginInstall`.

make [Shougo/vimproc.vim](https://github.com/Shougo/vimproc.vim) (required by [eagletmt/ghcmod-vim](https://github.com/eagletmt/ghcmod-vim) and some other useful plugins).

make [Valloric/YouCompleteMe](https://github.com/Shougo/vimproc.vim).

## Reset Emacs

Remove existing dotfiles:

    rm -rf ~/.emacs ~/.emacs.d

Restore dotfiles:

    ruby copy-dotfiles.rb
