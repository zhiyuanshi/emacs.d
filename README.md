# zhiyuanshi dotfiles

## Prerequisites

Set zsh as your login shell:

    chsh -s $(which zsh)

Set variables:

    export DOTFILES=~/Dropbox/Code/dotfiles

## Install Vim

Remove existing dotfiles:

    rm -rf ~/.vimrc ~/.vim

Install [Vundle](https://github.com/gmarik/Vundle.vim).

Restore dotfiles:

    cp $DOTFILES/.vimrc ~

Open Vim and run `:PluginInstall`.

make [Shougo/vimproc.vim](https://github.com/Shougo/vimproc.vim) (required by [eagletmt/ghcmod-vim](https://github.com/eagletmt/ghcmod-vim) and some other useful plugins).

make [Valloric/YouCompleteMe](https://github.com/Shougo/vimproc.vim).

## Install Emacs

Remove existing dotfiles:

    rm -rf ~/.emacs ~/.emacs.d

Install Emacs [Prelude](https://github.com/bbatsov/prelude#fast-forward):

    curl -L http://git.io/epre | sh

Back up the latest pristine `prelude-modules.el`:

    cp ~/.emacs.d/prelude-modules.el $DOTFILES/.emacs.d/prelude-modules.el.original

Open Emacs to allow Prelude be automatically initialized. This may take some time.

Restore dotfiles:

    cp $DOTFILES/.emacs ~
    cp $DOTFILES/.emacs.d/prelude-modules.el ~/.emacs.d/prelude-modules.el
