# dotfiles

# Prerequisites

Set Zsh as your login shell:

    sudo apt-get update && sudo apt-get install zsh
    [ -d ~/.oh-my-zsh ] || curl -L http://install.ohmyz.sh | sh
    chsh -s $(which zsh)

## Set up

    git clone https://github.com/zhiyuanshi/dotfiles.git
    cd dotfiles
    rake setup

You can safely run `rake setup` multiple times to update:

    rake setup