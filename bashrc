# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

export GPGKEY=42C866A3

function switchToWorkspace {
    workspace=$1

    unset PYTHONPATH
    echo "Cleaned PYTHONPATH env var.."

    cd /home/lukasz/workspace/${workspace}/src
    
    if [ -f /home/lukasz/workspace/${workspace}/environment/setup_environment.sh ]; then
        source /home/lukasz/workspace/${workspace}/environment/setup_environment.sh
    fi
    
    source /home/lukasz/workspace/${workspace}/environment/bin/activate
    echo "To deactivate this python virtual env type: deactivate"
}

function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

# extract stash size from git repo in CWD
function git_stash_size {
 lines=$(git stash list -n 100 2> /dev/null) || return
 if [ "${#lines}" -gt 0 ]
 then
   count=$(echo "$lines" | wc -l | sed 's/^[ \t]*//') # strip tabs
   echo "[S#"${count#}"]"
 fi
}

function set_prompt {
    PS1="[\$(date +%H:%M)]"

    PS1="$PS1\u@\h:\W\$(parse_git_branch)\$(git_stash_size)$ "
    unset color_prompt force_color_prompt
}

# Change to most recently used directory:
if [ -f ~/.lastdir ]; then
    cd "`cat ~/.lastdir`"
fi
 
export LASTDIR="/"
 
function prompt_command {
 
  # Remember where we are:
  pwd > ~/.lastdir
 
  # Record new directory on change.
  newdir=`pwd`
  if [ ! "$LASTDIR" = "$newdir" ]; then
 
    # List contents:
    ls -t

    # Activate pythons' virtualenv
  
    path_to_activator="bin/activate"
    VENV_DIRNAMES="env
    pyenv
    py-env"

    for DIRNAME in $VENV_DIRNAMES
    do
      FULL_PATH="$newdir/$DIRNAME/$path_to_activator"
      CURRENT_VIRTUAL_ENV="$VIRTUAL_ENV"
      
      if [ -e $FULL_PATH ] && [ "$FULL_PATH" != "$CURRENT_VIRTUAL_ENV" ]; then
        deactivate >& /dev/null && echo "Deactivated virtualenv: $CURRENT_VIRTUAL_ENV"
        source $FULL_PATH
        echo "Sourcing virtualenv from: $FULL_PATH"
        break
      fi
    done
    fi
  export LASTDIR=$newdir 
}

# find all files containing given text pattern
function fcontaining {
    ROOTDIR="$1"

    if [ -n "$3" ]; then
        FILE_PATTERN="$2"
        TEXTSTR="$3"
    else
        FILE_PATTERN="*"
        TEXTSTR="$2"
    fi

    find "$ROOTDIR" -iname "$FILE_PATTERN" -exec grep -l "$TEXTSTR" {} \;
}


# use rsync to synchronize two paths
function sync {
  SRC="$1"
  DEST="$2"
  ARGS="${*:3}"

  rsync -arvuz $ARGS $SRC $DEST
}
 
export PROMPT_COMMAND="prompt_command"

set_prompt

# disable control flow
stty -ixon


# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=10000
HISTFILESIZE=20000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi


# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
    alias watch='watch --color'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi


### APPLICATIONS SECTION
export PATH=$PATH:~/.cabal/bin:~/.xmonad/bin


