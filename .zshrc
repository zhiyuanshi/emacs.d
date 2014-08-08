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

plugins=(bundler cabal gem git heroku mercurial rails rake rbenv ruby sbt scala)

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

# Invert behavior of Fn key on Apple keyboard
# https://help.ubuntu.com/community/AppleKeyboard#Change_Function_Key_behavior
# echo 2 > /sys/module/hid_apple/parameters/fnmode

# Preferred editor for local and remote sessions
EMACSCLIENT="emacsclient --create-frame --no-wait --alternate-editor=\"\""

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR="vim"
else
  export EDITOR="$EMACSCLIENT"
fi

export CODE=$HOME/Dropbox/Code

# Aliases
alias cl="clear"
alias open="xdg-open"

alias v="gvim 2> /dev/null"
alias e="$EMACSCLIENT"

alias code="cd $CODE"

export DOTFILES=$CODE/dotfiles

alias dotfiles="cd $DOTFILES"
alias dotvim="$EDITOR $DOTFILES/.vimrc"
alias dotemacs="$EDITOR $DOTFILES/.emacs"
alias dotzsh="$EDITOR $DOTFILES/.zshrc"

# The default PATH defined in /etc/environment
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

# opam
export PATH="$HOME/.opam/4.01.0/bin:$PATH"
eval `opam config env`

# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# cabal
export PATH="$HOME/.cabal/bin:$PATH"