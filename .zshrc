# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

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

plugins=(bundler cabal gem git heroku mercurial rails rake ruby rvm sbt scala)

# PROMPT="
# %c%# "

source $ZSH/oh-my-zsh.sh        # Required

#=============================================================================
# USER CONFIGURATION

# Clear "Recently Used"
echo > $HOME/.local/share/recently-used.xbel
touch  $HOME/.local/share/recently-used.xbel
echo "gtk-recent-files-max-age=0" > $HOME/.gtkrc-2.0

# Disable ThinkPad TrackPoint
xinput -set-prop "TPPS/2 IBM TrackPoint" "Device Enabled" 0

# For RVM to work in ZSH
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

# Invert behavior of Fn key on Apple keyboard
# https://help.ubuntu.com/community/AppleKeyboard#Change_Function_Key_behavior
# echo 2 > /sys/module/hid_apple/parameters/fnmode

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR="vim"
else
  export EDITOR="v"
fi

#-----------------------------------------------------------------------------
# ENV VARIABLES

export PATH=$PATH:"/home/zhiyuan/.opam/4.01.0/bin:/home/zhiyuan/.rvm/gems/ruby-1.9.3-p484/bin:/home/zhiyuan/.rvm/gems/ruby-1.9.3-p484@global/bin:/home/zhiyuan/.rvm/rubies/ruby-1.9.3-p484/bin:/home/zhiyuan/.rvm/bin:/home/zhiyuan/.cabal/bin:/usr/lib/lightdm/lightdm:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"

export CODE=$HOME/Dropbox/Code
export DOTFILES=$CODE/dotfiles

#-----------------------------------------------------------------------------
# ALIASES

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

alias cl="clear"
alias open="xdg-open"

alias v="gvim 2> /dev/null"
alias e="emacs"

alias code="cd $CODE"
alias dotfiles="cd $DOTFILES"

alias dotvim="$EDITOR $DOTFILES/.vimrc"
alias dotemacs="$EDITOR $DOTFILES/.emacs"
alias dotzsh="$EDITOR $DOTFILES/.zshrc"

# Deprecated, since ag is noticeably faster than ack
alias ack="ack-grep"
