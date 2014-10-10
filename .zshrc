
export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="mgutz"

DISABLE_UPDATE_PROMPT="true"

# DISABLE_UNTRACKED_FILES_DIRTY="true"

plugins=(bundler cabal capistrano gem git heroku mercurial rails rake rbenv ruby sbt scala)

source $ZSH/oh-my-zsh.sh        # Required

# PROMPT="
# %c%# "

echo > "$HOME/.local/share/recently-used.xbel"
touch  "$HOME/.local/share/recently-used.xbel"
echo "gtk-recent-files-max-age=0" > "$HOME/.gtkrc-2.0"

xinput -set-prop "TPPS/2 IBM TrackPoint" "Device Enabled" 0

# echo 2 > /sys/module/hid_apple/parameters/fnmode

export EMACSCLIENT="emacsclient --create-frame --no-wait --alternate-editor=''"

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR="vim"
else
  export EDITOR="$EMACSCLIENT"
fi

export DROPBOX="$HOME/Dropbox"
export CODE="$DROPBOX/Code"
export DOTFILES="$CODE/dotfiles"
export SCRIPTS="$CODE/scripts"

alias code="cd $CODE"
alias dotfiles="cd $DOTFILES"

function reload() {
  cp $DOTFILES/.zshrc ~
  echo "Copied $DOTFILES/.zshrc to ~"

  source ~/.zshrc
  echo "Reloaded .zshrc from ~"
}

alias cl="clear"
alias open="xdg-open"

alias v="gvim 2> /dev/null"
alias e="$EMACSCLIENT"
alias s="subl"

function runjava() {
  javac $1
  class_name=${${1}%.java}
  shift
  java $class_name $@
  rm $class_name.class
}

alias git-pull-all="$SCRIPTS/git-pull-all.sh"

alias hlog="hg log --template '#{rev} {date|isodate} {desc|firstline}\n' | less"

alias find-my-ip="ip addr show eth0 | grep inet | awk '{ print $2; }' | sed 's/\/.*$//'"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

export DATABASE_URL=postgres:///$(whoami)

nginx-load-conf-and-restart() {
  sudo cp $DOTFILES/nginx.conf /etc/nginx/nginx.conf
  sudo nginx -t
  sudo service nginx restart
}

export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

export PATH="$HOME/.opam/4.01.0/bin:$PATH"
eval `opam config env`

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

export PATH="$HOME/.cabal/bin:$PATH"

if [ -f ~/.fzf.zsh ]; then
  source ~/.fzf.zsh
else
  echo '~/.fzf.zsh not found'
fi
