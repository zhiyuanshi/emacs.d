# dotfiles

My personal Emacs, Vim, Zsh (and etc.) configuration primarily intended for Haskell and Ruby development.

# Prerequisites

Set Zsh as your login shell:

    if ! command -v zsh >/dev/null; then
      sudo apt-get update && sudo apt-get install zsh
    fi
    chsh -s $(which zsh)

# Set up

    git clone https://github.com/zhiyuanshi/dotfiles.git
    cd dotfiles
    rake up

You can safely run `rake up` multiple times to update:

    rake up