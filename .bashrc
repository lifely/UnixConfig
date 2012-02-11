#Make sure all terminals save history
unset LC_CTYPE
shopt -s histappend
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

#Increase history size
export HISTSIZE=1000
export HISTFILESIZE=1000

#Use GREP color features by default
export GREP_OPTIONS='--color=auto'

if [[ `which dircolors` ]]
then
eval `dircolors -b $HOME/.dircolors`
fi

if [[ `which gdircolors` ]]
then
eval `gdircolors -b $HOME/.dircolors`
fi

ulimit -c unlimited
source ~/.bash_profile