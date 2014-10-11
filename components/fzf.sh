if defined fzf; then
  source ~/.fzf.zsh
else
  git clone https://github.com/junegunn/fzf.git ~/.fzf
  ~/.fzf/install
fi