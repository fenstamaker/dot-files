# Path to your oh-my-zsh installation.
export ZSH=/Users/fenstamaker/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="bureau"
#ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
ZSH_CUSTOM=~/.zsh_custom

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(aws brew common-aliases compleat git git-extras httpie jsontools lein mvn npm osx pip python ruby sbt scala sudo tldr wd)
# User configuration

export PATH="/Users/fenstamaker/anaconda3/bin:/Users/fenstamaker/Developer:/usr/local/go/bin:/usr/local/bin:/opt/local/bin:/opt/local/sbin:/Users/fenstamaker/Developer/maven/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/fenstamaker/Developer/adt/sdk/platform-tools:/Users/fenstamaker/Developer/adt/sdk/tools:/usr/local/mysql/bin:/usr/local/sbin"

# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

export EDITOR='vim'

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias arc="envdir ~/.envs/arc"
alias syd="envdir ~/.envs/syd"
alias awscut="cut -d \" \" -f 3-"
alias reload=". ~/.zshrc && echo 'ZSH config reloaded from ~/.zshrc'"
alias start_mongo="mongod --config /usr/local/etc/mongod.conf"
[[ -s "$HOME/.local/share/marker/marker.sh" ]] && source "$HOME/.local/share/marker/marker.sh"

alias dc="docker-compose"
alias dcu="docker-compose up"
alias dcb="docker-compose build"

alias dockerclean="docker rm -v $(docker ps -a -q -f status=exited) && docker rmi $(docker images -f "dangling=true" -q)"
alias dcc="dockerclean"
alias subtree="git pull -s subtree"

alias gcm="git commit -m"
alias cheat="tldr"
alias manuel="/usr/bin/man"
alias man="tldr"
alias json="jq"


alias vpn="sudo openconnect --juniper --user=fenstamakerg --authgroup=TWP-main ra.washpost.com"

# alt <- moves back a word
# alt -> moves forward a word
bindkey -e
bindkey '\e\e[C' forward-word
bindkey '\e\e[D' backward-word


source /Users/fenstamaker/Developer/git-subrepo/.rc
fpath=('/Users/fenstamaker/Developer/git-subrepo/share/zsh-completion' $fpath)


shift-arrow() {
  ((REGION_ACTIVE)) || zle set-mark-command
  zle $1
}
for key kcap seq widget (
    left  LFT $'\e[1;2D' backward-char
    right RIT $'\e[1;2C' forward-char
    up    ri  $'\e[1;2A' up-line-or-history
    down  ind $'\e[1;2B' down-line-or-history
  ) {
  eval "shift-$key() shift-arrow $widget"
  zle -N shift-$key
  bindkey ${terminfo[k$kcap]-$seq} shift-$key
}
