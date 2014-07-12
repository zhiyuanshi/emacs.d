# Vim

## Install

```bash
rm ~/.vimrc && rm -rf ~/.vim/ && cp ~/Dropbox/Code/dotfiles/.vimrc ~
```

Install [Vundle](https://github.com/gmarik/Vundle.vim)

Open Vim and run `:PluginInstall`

make [Shougo/vimproc.vim](https://github.com/Shougo/vimproc.vim) (required by [eagletmt/ghcmod-vim](https://github.com/eagletmt/ghcmod-vim))

make [Valloric/YouCompleteMe](https://github.com/Shougo/vimproc.vim)

# Emacs

## Install

```bash
rm ~/.emacs && rm -rf ~/.emacs.d/
```

[Fast-forward install](https://github.com/bbatsov/prelude#fast-forward) Prelude:

```bash
curl -L http://git.io/epre | sh
```

```bash
cp ~/.emacs.d/prelude-modules.el ~/Dropbox/Code/dotfiles/.emacs.d/prelude-modules.el.original
```

Open Emacs and Prelude will be automatically initialized.

```bash
cp ~/Dropbox/Code/dotfiles/.emacs ~
cp ~/Dropbox/Code/dotfiles/prelude-modules.el ~/.emacs.d
```

The evil package that comes with Prelude doesn't seem to work. If that's the case, open Emacs and do a manual [install](http://www.emacswiki.org/emacs/Evil#toc1) by:

```bash
M-x package-install RET evil RET
```