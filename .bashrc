#Make sure all terminals save history
unset LC_CTYPE
shopt -s histappend
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

#Increase history size
export HISTSIZE=1000
export HISTFILESIZE=1000

#Use GREP color features by default
export GREP_OPTIONS='--color=auto'

eval `dircolors -b $HOME/.dircolors`

ulimit -c unlimited
source ~/.bash_profile