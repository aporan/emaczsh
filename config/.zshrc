# Path to your oh-my-zsh installation.
ME=$(whoami)
export ZSH=/home/$ME/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

## Autosuggestions
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=1"

## Terminal Prompt Customization
ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[white]%}<%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[white]%}> %{$fg[yellow]%}✗"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[white]%}>"

local ret_status="%(?:%{$fg_bold[green]%} ➜ :%{$fg_bold[red]%} ➜ %s)"
PROMPT='%{$fg_bold[red]%}%m %{$fg_bold[yellow]%}[%{$fg_bold[white]%}%1~%{$fg_bold[yellow]%}]$ret_status %{$reset_color%}$(git_prompt_info)'

## before using this stub do:
##   "dircolors -p > ~./dircolors"
##   change 'OTHER_WRITABLE' colors to something different'
## customize terminal directory colors for 'everybody or other writable'
if [[ -f ~/.dircolors ]] ; then
    eval $(dircolors -b ~/.dircolors)
fi

# add aliases
if [ -f ~/.aliases ]; then
     . ~/.aliases
fi

# add pythonpath to make python folders to project. E.g. 'PYTHONPATH='/path/to/dir/'
if [ -f ~/.pythonpath ]; then
     . ~/.pythonpath
fi

# add systempath for projects. E.g. 'PATH='/path/to/dir/'
if [ -f ~/.systempath ]; then
     . ~/.systempath
fi

# customize tab completion colors which follows the LS_COLORS from dircolors used above
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"
autoload -Uz compinit
compinit

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# zsh completion directory
fpath=(~/.zsh/completion $fpath)
