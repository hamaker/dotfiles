echo $BASH_SOURCE
PATH="/usr/local/bin:/Users/niels/.rbenv/bin:/usr/local/share/npm/bin/:$PATH"
PATH="/Library/Developer/AndroidDeveloperTools/sdk/tools:$PATH"
PATH="/Library/Developer/AndroidDeveloperTools/sdk/platform-tools:$PATH"
PATH="./bin:$PATH"
eval "$(rbenv init -)"
source ~/.rbenv/completions/rbenv.bash
source ~/dotfiles/bash_colours.bash
 
# rbenv version | sed -e 's/ .*//'

if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi

export DYLD_LIBRARY_PATH="/usr/local/oracle/instantclient_10_2" 
export SQLPATH="/usr/local/oracle/instantclient_10_2" 
export TNS_ADMIN="/usr/local/oracle/instantclient_10_2" 
export PATH=$PATH:$DYLD_LIBRARY_PATH
export NODE_PATH='/usr/local/lib/node'
export BUNDLER_EDITOR='vim'
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8


alias r='bundle exec rails'
alias rc='bundle exec rails c'
alias be='bundle exec'
alias spec='bundle exec rspec'
alias b='bundle'
alias migrate='bundle exec rake db:migrate && bundle exec rake db:test:prepare'

alias ls='ls -G'
alias ll='ls -lhatr'
alias l='ls -l'
alias la='ls -a'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

alias grep='grep --color=auto'
alias psa='ps uax'
alias v='vim .'

alias g='git'
alias gp='git push'
alias gcom='git co master'
alias gcot='git co testing'
alias pr='powder restart'
alias ssh-add-brightin='ssh-add ~/.ssh/brightin.id_rsa'
alias gt='gmerge testing'
alias gm='gmerge master'
alias nerd='cd ~/nerd/'

#GIT_PS1_SHOWUPSTREAM="verbose"
GIT_PS1_SHOWDIRTYSTATE=true
GIT_PS1_SHOWSTASHSTATE=true
GIT_PS1_SHOWUPSTREAM="auto"
export PS1="\n$Red[\t] \$(rbenv version-name) \n$Green[\w]$BYellow \n\$( __git_ps1 '%s ')$Color_Off\$ "
#export PS1="\n\e[0;31m[\t]\e[m \e[0;31m \$(rbenv version-name) \e[m \n\e[0;32m[\w]\e[m \n\e[1;33m\$(__git_ps1)\e[m $ "
#PS1='\n\[\033[32m\]\u@\h\[\033[00m\]:\[\033[34m\]\w\[\033[31m\]$(__git_ps1)\[\033[00m\]\n\$ '

shopt -s histappend
export HISTCONTROL=erasedups

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
export PROMPT_COMMAND='history -a ; echo -ne "\033]0;`echo ${PWD##*/}`\007"'
export PGDATA='/usr/local/var/postgres'
poms () { open http://docs.poms.omroep.nl/media/$1 ;}
gmerge () {
  branch=`git describe --contains --all HEAD`
  git push origin HEAD
  git checkout $1 && git pull && git merge "$branch" && git push && git checkout "$branch"
}
  
