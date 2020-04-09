pathmunge () {
  if ! echo $PATH | egrep -q "(^|:)$1($|:)" ; then
    if [ "$2" = "after" ] ; then
      PATH=$PATH:$1
    else
      PATH=$1:$PATH
    fi
  fi
}

pathmunge /Applications/Postgres.app/Contents/Versions/9.6/bin after
pathmunge "$HOME/.cargo/bin"
pathmunge "${KREW_ROOT:-$HOME/.krew}/bin"
pathmunge $HOME/.bin after

export PATH

