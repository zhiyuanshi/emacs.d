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

Set up [Vundle](https://github.com/gmarik/Vundle.vim):

    git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

Restore dotfiles:

    ruby "$DOTFILES/copy_dotfiles.rb"

Install plugins:

    gvim +PluginInstall +qall

Build [Valloric/YouCompleteMe](https://github.com/Valloric/YouCompleteMe):

    cd ~/.vim/bundle/YouCompleteMe && ./install.sh --clang-completer

Build [Shougo/vimproc.vim](https://github.com/Shougo/vimproc.vim):

    cd ~/.vim/bundle/vimproc.vim && make

## Reset Emacs

Remove existing configuration and packages:

    rm -rf ~/.emacs ~/.emacs.d

Restore dotfiles:

    ruby "$DOTFILES/copy_dotfiles.rb"
