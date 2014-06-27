ZSH=$HOME/.oh-my-zsh

source $ZSH/oh-my-zsh.sh

export PATH=$PATH:"/home/zhiyuan/.opam/4.01.0/bin:/home/zhiyuan/.rvm/gems/ruby-1.9.3-p484/bin:/home/zhiyuan/.rvm/gems/ruby-1.9.3-p484@global/bin:/home/zhiyuan/.rvm/rubies/ruby-1.9.3-p484/bin:/home/zhiyuan/.rvm/bin:/home/zhiyuan/.cabal/bin:/usr/lib/lightdm/lightdm:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"

# Clear `Recently Used`
echo > ~/.local/share/recently-used.xbel
touch  ~/.local/share/recently-used.xbel
echo 'gtk-recent-files-max-age=0' > ~/.gtkrc-2.0

# Disable ThinkPad TrackPoint
xinput -set-prop "TPPS/2 IBM TrackPoint" "Device Enabled" 0

# For RVM to work in ZSH
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

PROMPT="
%c%# "

plugins=(git mercurial ruby rails bundler gem heroku)

alias clr='clear'
alias t='tree'
alias open='xdg-open'
alias ack='ack-grep'

alias v='gvim 2> /dev/null'
alias e='emacs'

# Invert behavior of Fn key on Apple keyboard
# https://help.ubuntu.com/community/AppleKeyboard#Change_Function_Key_behavior
# echo 2 > /sys/module/hid_apple/parameters/fnmode
