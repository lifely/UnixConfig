#Make sure all terminals save history
shopt -s histappend
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

#Increase history size
export HISTSIZE=1000
export HISTFILESIZE=1000

ulimit -c unlimited
source ~/.bash_profile