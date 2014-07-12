# Vim

## Install

1. Clear:

   ```bash
   $ rm ~/.vimrc && rm -rf ~/.vim/
   ```

2. Install [Vundle](https://github.com/gmarik/Vundle.vim):

   ```bash
   $ git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
   ```

3. Restore `.vimrc`:

   ```bash
   $ cp ~/Dropbox/Code/dotfiles/.vimrc ~
   ```

4. Open Vim and run `:PluginInstall`

5. make [Shougo/vimproc.vim](https://github.com/Shougo/vimproc.vim) (required by [eagletmt/ghcmod-vim](https://github.com/eagletmt/ghcmod-vim))

6. make [Valloric/YouCompleteMe](https://github.com/Shougo/vimproc.vim)

# Emacs

## Install

1. Clear:

   ```bash
   $ rm ~/.emacs && rm -rf ~/.emacs.d/
   ```

2. Install [Prelude](https://github.com/bbatsov/prelude#fast-forward):

   ```bash
   $ curl -L http://git.io/epre | sh
   ```

3. Back up `prelude-modules.el`:

   ```bash
   $ cp ~/.emacs.d/prelude-modules.el ~/Dropbox/Code/dotfiles/.emacs.d/prelude-modules.el.original
   ```

4. Open Emacs. Prelude will be automatically initialized.

5. Restore `.emacs` and `prelude-modules.el`:

   ```bash
   $ cp ~/Dropbox/Code/dotfiles/.emacs ~
   $ cp ~/Dropbox/Code/dotfiles/prelude-modules.el ~/.emacs.d
   ```

6. (Optional) The evil package that comes with Prelude doesn't seem to work. If that's the case, open Emacs and do a [manual install](http://www.emacswiki.org/emacs/Evil#toc1) by:

   ```bash
   M-x package-install RET evil RET
   ```
