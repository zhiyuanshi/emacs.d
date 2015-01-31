# .zshrc

# Make sure oh-my-zsh is installed
[ -d ~/.oh-my-zsh ] || curl -L http://install.ohmyz.sh | sh

# Provided by oh-my-zsh

# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="mgutz"

# Automatically update zsh itself without prompting me.
DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Plugins
plugins=(bundler cabal capistrano gem git heroku mercurial rails rake rbenv ruby sbt scala)

# Source oh-my-zsh.sh
source $ZSH/oh-my-zsh.sh        # Required

# User configuration
# Set prompt
# PROMPT="
# %c%# "

# Tasks on Zsh start-up
# Clear "Recently Used"
if [ -e ~/.local/share/recently-used.xbel ]; then
  echo > ~/.local/share/recently-used.xbel
  touch  ~/.local/share/recently-used.xbel
fi

[ -f ~/.gtkrc-2.0 ] && echo "gtk-recent-files-max-age=0" > ~/.gtkrc-2.0

# Disable ThinkPad TrackPoint
if which xinput &>/dev/null; then
  xinput -set-prop "TPPS/2 IBM TrackPoint" "Device Enabled" 0
fi

# Invert behavior of Fn key on Apple keyboard
# https://help.ubuntu.com/community/AppleKeyboard#Change_Function_Key_behavior
# echo 2 > /sys/module/hid_apple/parameters/fnmode

# Preferred editor for local and remote sessions
export EMACSCLIENT="emacsclient --create-frame --no-wait --alternate-editor=''"

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR="vim"
else
  export EDITOR="$EMACSCLIENT"
fi

# Shortcuts to directories
# http://stackoverflow.com/a/23259585/1895366
export ZSHRC=${(%):-%N}
export DOTFILES=$(dirname $(readlink -f $ZSHRC))

# Aliases
alias cl="clear"
alias open="xdg-open"

alias v="gvim 2> /dev/null"
alias e="$EMACSCLIENT"
alias s="subl"
alias a="atom"

# Running Java with one command
# Something similar to =runhaskell=, why didn't we have one?
# Extracted from the following Japanese blog post:
# http://matsu-chara.hatenablog.com/entry/2014/05/17/210000
# Also, see:
# http://itchyny.hatenablog.com/entry/20130227/1361933011
function runjava() {
  javac $1
  class_name=${${1}%.java}
  shift
  java $class_name $@
  rm $class_name.class
}

# Git & Mercurial
alias hlog="hg log --template '#{rev} {date|isodate} {desc|firstline}\n' | less"

# Utils
# https://www.digitalocean.com/community/tutorials/how-to-install-nginx-on-ubuntu-14-04-lts

alias find-my-ip="ip addr show eth0 | grep inet | awk '{ print $2; }' | sed 's/\/.*$//'"
alias upgrade-system="sudo apt-get update && sudo apt-get -y dist-upgrade"

# Functions
disable-post-installation-script() {
  sudo mv "/var/lib/dpkg/info/$1.postinst" "/var/lib/dpkg/info/$1.postinst.original"
}

# nvm (Node Version Manager)
export NVM_DIR="~/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

# PostgreSQL
# https://devcenter.heroku.com/articles/heroku-postgresql#local-setup
# Once Postgres is installed and you can connect, you'll need to export the
# DATABASE_URL environment variable for your app to connect to it when running
# locally. E.g.:
export DATABASE_URL=postgres:///$(whoami)

# nginx
# http://railscasts.com/episodes/357-adding-ssl
nginx-load-conf-and-restart() {
  sudo cp $DOTFILES/nginx.conf /etc/nginx/nginx.conf
  sudo nginx -t
  sudo service nginx restart
}

# PATH
# Defined in =/etc/environment=
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

if which opam &>/dev/null ; then
  export PATH="~/.opam/4.01.0/bin:$PATH"
  source ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
  eval `opam config env`
fi

function defined { command -v $1 &>/dev/null }
function require { source "$DOTFILES/$1.sh" }

# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# fzf, depends on Ruby
# A general-purpose fuzzy finder for your shell
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# cabal
export PATH=~/.cabal/bin:$PATH

# Activator for the Play Framework
# https://www.playframework.com/documentation/2.3.x/Installing
export PATH=$PATH:~/Applications/activator-1.2.12-minimal

# IntelliJ IDEA
export PATH=$PATH:~/Applications/idea-IU-139.1117.1/bin

# RubyMine
export PATH=$PATH:~/Applications/RubyMine-7.0.3/bin
