#!/bin/bash

# colored grep
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;33'

# colored ls
export LSCOLORS='Gxfxcxdxdxegedabagacad'

export LS_OPTIONS='--color=auto'
export CLICOLOR='Yes'

## Colors LS
if [[ `which dircolors` ]]
then
eval `dircolors -b $HOME/.dircolors`
fi

# Load the theme
if [[ $BASH_THEME ]]; then
    source "$BASH/themes/$BASH_THEME/$BASH_THEME.theme.bash"
fi

