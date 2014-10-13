# We don't like the Cabal packaged with haskell-platform.
# if [[ $(which cabal) == "/usr/bin/cabal" ]]; then
#   cabal update
#   cabal install cabal-install
# fi

export PATH="$HOME/.cabal/bin:$PATH"