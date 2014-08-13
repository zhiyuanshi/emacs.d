# zhiyuanshi dotfiles

## Prerequisites

1. Set zsh as your login shell:

        chsh -s $(which zsh)

2. Set the path to your local dotfiles directory if you haven't done so. In my case, that is:

        export DOTFILES="~/Dropbox/Code/dotfiles"

3. Clone the repository to get a local copy, if you don't have one:

        git clone https://github.com/zhiyuanshi/dotfiles.git "$DOTFILES"

## Reset Vim

1. Remove existing configuration and packages:

        rm -rf ~/.vimrc ~/.vim

2. Set up [Vundle](https://github.com/gmarik/Vundle.vim):

        git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

3. Restore dotfiles:

        ruby "$DOTFILES/copy_dotfiles.rb"

4. Install plugins:

        gvim +PluginInstall +qall

5. Build [YouCompleteMe](https://github.com/Valloric/YouCompleteMe):

        cd ~/.vim/bundle/YouCompleteMe && ./install.sh --clang-completer

6. Build [vimproc.vim](https://github.com/Shougo/vimproc.vim):

        cd ~/.vim/bundle/vimproc.vim && make

## Reset Emacs

1. Remove existing configuration and packages:

        rm -rf ~/.emacs ~/.emacs.d

2. Restore dotfiles:

        ruby "$DOTFILES/copy_dotfiles.rb"
