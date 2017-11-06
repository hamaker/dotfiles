# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt appendhistory autocd
setopt HIST_FIND_NO_DUPS
unsetopt beep
DIRSTACKSIZE=5
setopt pushdignoredups pushdsilent autopushd pushdminus
alias d='dirs -v'
bindkey -e
# End of lines configured by zsh-newuser-install
zstyle :compinstall filename '/home/niels/.zshrc'

autoload -Uz compinit promptinit
compinit
# promptinit; prompt gentoo
zstyle ':completion::complete:*' use-cache 1

source ~/.aliases
source ~/.zshprompt
source ~/.zshenv
source ~/.zshkeys

if [ "$SHELL" = '/bin/zsh' ]
then
  case $TERM in
    rxvt|*term|rxvt-unicode-256color)
      precmd() { print -Pn "\e]0;%~\a" }
      preexec () { print -Pn "\e]0;$1\a" }
      ;;
  esac
fi
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':completion:*:*:cdr:*:*' menu selection

function notes() {
  if [ ! -z "$1" ]; then
    # Using the "$@" here will take all parameters passed into
    # this function so we can place everything into our file.
    echo "$@" >> "$HOME/notes.md"
  else
    # If no arguments were passed we will take stdout and place
    # it into our notes instead.
    cat - >> "$HOME/notes.md"
  fi
}

function copy () {
  ${@} | xclip -selection clipboard -rmlastnl
}
