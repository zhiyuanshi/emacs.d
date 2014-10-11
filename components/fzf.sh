if ! defined fzf; then
  git clone https://github.com/junegunn/fzf.git ~/.fzf
  ~/.fzf/install
fi

source ~/.fzf.zsh