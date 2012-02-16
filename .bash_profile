#!/bin/bash

# Load RVM, if you are using it
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm
[[ -s /etc/profile.d/rvm.sh ]] && source /etc/profile.d/rvm.sh

# Add rvm gems and nginx to the path
export PATH=$PATH:~/.gem/ruby/1.8/bin:/opt/nginx/sbin
export PATH=$PATH:/usr/local/git/bin:/opt/local/bin:/opt/local/sbin

export PATH=$PATH:/usr/netsoul/sbin:/usr/netsoul/bin
export PATH=$PATH:/usr/kerberos/sbin:/usr/kerberos/bin
export PATH=$PATH:/usr/arla/sbin:/usr/arla/bin
export PATH=$PATH:/usr/local/sbin:/usr/local/bin
export PATH=$PATH:/usr/sbin:/usr/bin:/sbin:/bin
export PATH=$PATH:/usr/site/sbin:/usr/site/bin
export PATH=$PATH:/usr/school/bin/

# Path to the bash it configuration
export BASH=$HOME/.bash_it

# Lock and Load a custom theme file
# location /.bash_it/themes/
export BASH_THEME='rainbowbrite'

# Your place for hosting Git repos. I use this for private repos.
export GIT_HOSTING='git@git.domain.com'

# Set my editor and git editor
export EDITOR="emacs -nw"
export GIT_EDITOR='emacs -nw'

# Set the Term - 256 classy
export TERM="xterm-256color"

# Set the path nginx
export NGINX_PATH='/opt/nginx'

# Don't check mail when opening terminal.
unset MAILCHECK

# Change this to the path of your local jekyll root to use the jekyll aliases

export JEKYLL_LOCAL_ROOT="$HOME/Sites/jekyllsite"

# And change this to the remote server and root

export JEKYLL_REMOTE_ROOT="user@server:/path/to/jekyll/root"

# And, for the last of the jekyll variables, this is the formatting you use, eg: markdown,
# textile, etc. Basically whatever you use as the extension for posts, without the preceding dot

export JEKYLL_FORMATTING="markdown"

# Change this to your console based IRC client of choice.

export IRC_CLIENT='irssi'

# Set this to the command you use for todo.txt-cli

export TODO="t"

# Set vcprompt executable path for scm advance info in prompt (demula theme)
# https://github.com/xvzf/vcprompt
#export VCPROMPT_EXECUTABLE=~/.vcprompt/bin/vcprompt

export CNORM_PATH="/usr/local/share/cnorm_3.0"

# Load Bash It
source $BASH/bash_it.sh

## Colors LS
if [[ `which dircolors` ]]
then
eval `dircolors -b $HOME/.dircolors`
fi

if [[ `which gdircolors` ]]
then
eval `gdircolors -b $HOME/.dircolors`
fi
