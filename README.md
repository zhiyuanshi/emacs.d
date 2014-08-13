# zhiyuanshi dotfiles

## Prerequisites

Set zsh as your login shell:

    chsh -s $(which zsh)

Set the path to your local dotfiles directory if you haven't done so. In my case, that is:

    export DOTFILES="~/Dropbox/Code/dotfiles"

Clone the repository to get a local copy:

    git clone https://github.com/zhiyuanshi/dotfiles.git "$DOTFILES"

## Reset Vim

Remove existing configuration and packages:

    rm -rf ~/.vimrc ~/.vim

Install [Vundle](https://github.com/gmarik/Vundle.vim).

Restore dotfiles:

    ruby "$DOTFILES/copy_dotfiles.rb"

Open Vim and run `:PluginInstall`.

make [Shougo/vimproc.vim](https://github.com/Shougo/vimproc.vim) (required by [eagletmt/ghcmod-vim](https://github.com/eagletmt/ghcmod-vim) and some other useful plugins).

make [Valloric/YouCompleteMe](https://github.com/Shougo/vimproc.vim).

## Reset Emacs

Remove existing configuration and packages:

    rm -rf ~/.emacs ~/.emacs.d

Restore dotfiles:

    ruby "$DOTFILES/copy_dotfiles.rb"
