#!/bin/bash

# Don't check mail when opening terminal.
unset MAILCHECK

## macos catalina bash to zsh deprecation silence
export BASH_SILENCE_DEPRECATION_WARNING=1

## Adds phabricator, arcanist review path bins (used by witbe)
export PATH="$PATH:/Users/lifely/Developer/playground/arcanist/arcanist/bin"

### MacOS Configurations Only
if [ "$(uname -s)" == "Darwin" ]; then
  export LANG=en_US.UTF-8

  ## Disable bash warnings on bigsur / catalina
  export BASH_SILENCE_DEPRECATION_WARNING=1
``
  # CoreUtils path (colored ls)
  export PATH=$(brew --prefix coreutils)/libexec/gnubin:$PATH
  export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

  # man PATH
  export MANPATH="$(brew --prefix coreutils)/libexec/gnuman:$MANPATH"
  export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
fi

## Ruby Manager Configuration RVM OR NVM
  # Load RVM, if you are using it
#   [[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm
#   [[ -s /etc/profile.d/rvm.sh ]] && source /etc/profile.d/rvm.sh

#   export NVM_DIR="$HOME/.nvm"
#   [ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
#   [ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion"  # This loads nvm bash_completion

## Epitech Legacy Paths
    # export PATH=$PATH:~/.gem/ruby/1.8/bin:/opt/nginx/sbin
    # export PATH=$PATH:/usr/local/git/bin:/opt/local/bin:/opt/local/sbin
    # export PATH=$PATH:/usr/netsoul/sbin:/usr/netsoul/bin
    # export PATH=$PATH:/usr/kerberos/sbin:/usr/kerberos/bin
    # export PATH=$PATH:/usr/arla/sbin:/usr/arla/bin
    # export PATH=$PATH:/usr/sbin:/usr/bin:/sbin:/bin
    # export PATH=$PATH:/usr/site/sbin:/usr/site/bin
    # export PATH=$PATH:/usr/school/bin

# Path to the bash it configuration
export BASH=$HOME/.bash_it

# Lock and Load a custom theme file
# location /.bash_it/themes/
export BASH_THEME='rainbowbrite'


# Set my editor and git editor
export EDITOR="emacs -nw"
export GIT_EDITOR='emacs -nw'

# Set the Term - 256 classy
export TERM="xterm-256color"

#Use GREP color features by default
export GREP_OPTIONS='--color=auto'


# Change this to your console based IRC client of choice.
export IRC_CLIENT='irssi'

# Set vcprompt executable path for scm advance info in prompt (demula theme)
# https://github.com/xvzf/vcprompt
#export VCPROMPT_EXECUTABLE=~/.vcprompt/bin/vcprompt

export CNORM_PATH="/usr/local/share/cnorm_3.0"

# Load Bash It
source $BASH/bash_it.sh

# Load ASDF (Version Manager)
. $(brew --prefix asdf)/asdf.sh
. $(brew --prefix asdf)/etc/bash_completion.d/asdf.bash

unset LC_CTYPE
