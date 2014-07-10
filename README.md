# Emacs

## Install

Remove existing local Emacs configurations and packages:

```bash
rm -rf ~/.emacs*
```

[Fast-forward install](https://github.com/bbatsov/prelude#fast-forward) Prelude:

```bash
curl -L http://git.io/epre | sh
```

Backup pristine configurations:

```bash
export DOTFILES_DIR=~/Dropbox/Code/dotfiles
cp ~/.emacs.d/prelude-modules.el $DOTFILES_DIR
```

Now, open Emacs to let Prelude be initialized.

Apply custom configurations:

```bash
cp $DOTFILES_DIR/.emacs ~
cp $DOTFILES_DIR/prelude-modules.el ~/.emacs.d
```

The evil package that comes with Prelude doesn't seem to work. If that's the case, open Emacs and do a manual [install](http://www.emacswiki.org/emacs/Evil#toc1) by:

```bash
M-x package-install RET evil RET
```

Done.
