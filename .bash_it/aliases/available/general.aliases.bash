#!/bin/bash

# epitech
alias norme=/u/all/astek/public/norme/norme
alias valgrind="valgrind --tool=memcheck --leak-check=yes --show-reachable=yes"
alias pullAFS="git checkout-index -f -a --prefix=/u/all/di-mar_j/rendu/codeworker/"
alias pcat="pygmentize -g"
alias codeworker="/usr/local/bin/codeworker -nologo"

# List directory contents
alias sl=ls
alias ls='ls -Gh'        # Compact view, show colors, human readable
alias la='ls -AFh'       # Compact view, show hidden
alias ll='ls -lh'
alias l='ls -alh'
alias l1='ls -1h'

alias _="sudo"

if [ $(uname) = "Linux" ]
then
	alias ls="ls --color=always -h"
fi

alias c='clear'
alias k='clear'
alias cls='clear'

alias edit="$EDITOR"
alias pager="$PAGER"

alias q="exit"

alias irc="$IRC_CLIENT"

alias rb="ruby"

# Pianobar can be found here: http://github.com/PromyLOPh/pianobar/

alias piano="pianobar"

alias ..='cd ..'        # Go up one directory
alias ...='cd ../..'    # Go up two directories
alias -- -="cd -"       # Go back

# Shell History
alias h='history'

# Tree
alias tree="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'"

# Directory
alias	md='mkdir -p'
alias	rd=rmdir

# show / hide hidden files
alias showhidden="defaults write com.apple.finder AppleShowAllFiles TRUE; killall Finder"
alias hidehidden="defaults write com.apple.finder AppleShowAllFiles FALSE; killall Finder"
# display IP address
alias myip="echo ethernet:; ipconfig getifaddr en0; echo wireless:; ipconfig getifaddr en1"

# http://snippets.dzone.com/posts/show/2486
alias killsvn="find . -name ".svn" -type d -exec rm -rf {} \;"

function aliases-help() {
  echo "Generic Alias Usage"
  echo
  echo "  sl      = ls"
  echo "  ls      = ls -G"
  echo "  la      = ls -AF"
  echo "  ll      = ls -al"
  echo "  l       = ls -a"
  echo "  c/k/cls = clear"
  echo "  ..      = cd .."
  echo "  ...     = cd ../.."
  echo "  -       = cd -"
  echo "  h       = history"
  echo "  md      = mkdir -p"
  echo "  rd      = rmdir"
  echo "  editor  = $EDITOR"
  echo "  pager   = $PAGER"
  echo "  piano   = pianobar"
  echo "  q       = exit"
  echo "  irc     = $IRC_CLIENT"
  echo "  md      = mkdir -p"
  echo "  rd      = rmdir"
  echo "  rb      = ruby"
  echo
}
