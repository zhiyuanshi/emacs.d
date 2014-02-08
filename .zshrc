ZSH=$HOME/.oh-my-zsh

source $ZSH/oh-my-zsh.sh

export PATH="/home/zhiyuan/.opam/4.01.0/bin:/home/zhiyuan/.rvm/gems/ruby-1.9.3-p484/bin:/home/zhiyuan/.rvm/gems/ruby-1.9.3-p484@global/bin:/home/zhiyuan/.rvm/rubies/ruby-1.9.3-p484/bin:/home/zhiyuan/.rvm/bin:/home/zhiyuan/.cabal/bin:/usr/lib/lightdm/lightdm:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"

# Clear `Recently Used`
echo > ~/.local/share/recently-used.xbel
touch  ~/.local/share/recently-used.xbel
echo 'gtk-recent-files-max-age=0' > ~/.gtkrc-2.0

# Disable ThinkPad TrackPoint
xinput -set-prop "TPPS/2 IBM TrackPoint" "Device Enabled" 0

# For RVM to work in ZSH
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

cd ~/Dropbox/Dev

PROMPT="
%c%% "

plugins=(git bundler gem heroku)

alias ack='ack-grep'
alias cr='clear'
alias gc='google-chrome &'
alias gg='gitg 2> /dev/null &'
alias gl='git log'
alias gp='git push origin master &'
alias gpf='git push -f origin master &'
alias gpull='git pull origin master &'
alias gs='git status'
alias n='nautilus'
alias open='xdg-open'
alias t='tree'
alias v='gvim 2> /dev/null'
