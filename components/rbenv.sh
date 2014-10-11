if ! defined rbenv; then
  git clone https://github.com/sstephenson/rbenv.git ~/.rbenv

  # Install ruby-build, which provides the rbenv install command
  git clone https://github.com/sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build
fi

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"