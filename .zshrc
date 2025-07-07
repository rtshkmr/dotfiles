############################################################################
# This file is my personal .zshrc config.                                  #
#                                                                          #
# Last update tag:                                                         #
# - fresh setup on macos after the thing bricked on me on <2024-05-03 Fri> #
#                                                                          #
# TODOs:                                                                   #
# 1. explore setup scripts for macos & arch                                #
############################################################################

# Thefuck configuration
# This enables the 'thefuck' command to correct previous console commands.
eval $(thefuck --alias)

# Ruby configuration
# Initialize rbenv for Ruby version management.
eval "$(rbenv init - zsh)"
source /opt/homebrew/opt/chruby/share/chruby/chruby.sh # Load chruby
source /opt/homebrew/opt/chruby/share/chruby/auto.sh   # Auto switch Ruby versions
# chruby ruby-3.1.3 # Set the default Ruby version

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"
export DOOMDIR="$HOME/.config/doom"
export ORGDIR="$HOME/org"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/rtshkmr/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/rtshkmr/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/rtshkmr/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/rtshkmr/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<



# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# This allows for faster prompt rendering and a more responsive shell experience.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi



# Set name of the theme to load.
# If set to "random", it will load a random theme each time oh-my-zsh is loaded.
ZSH_THEME="powerlevel10k/powerlevel10k"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
zstyle ':omz:update' frequency 7

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  git
  thefuck
  zbell
  colored-man-pages
  web-search
  zsh-interactive-cd
  catimg
)

# Source Oh My Zsh
# This line initializes Oh My Zsh with the configurations set above.
source $ZSH/oh-my-zsh.sh

# User configuration

# ---------- PATHS RELATED  VARS ----------------
# If you come from bash you might have to change your $PATH.
# Add custom script directories to your PATH.
export PATH="$HOME/org/scripts/:$PATH" # script bins @ org root, for private scripts
export PATH="$HOME/.config/emacs/bin:$PATH" # adds doom emacs bins
export PATH="$HOME/Library/Python/3.8/bin:$PATH" # Python binaries
export PATH="/opt/homebrew/opt/cython/bin:$PATH" # Cython binaries
export PATH="$HOME/.local/bin:$PATH" # to add local bins
export PATH="$HOME/elixir-ls-v0.22.1:$PATH" # elixir ls -- NOTE: likely needs to be continually updated upon version bumps
export PATH="/opt/homebrew/opt/libpq/bin:$PATH" # PostgreSQL binaries
export PATH="$HOME/go/bin:$PATH" # globally available go binaries

# ---------- PERSONAL ALIASES ----------
# Set personal aliases, overriding those provided by oh-my-zsh libs, plugins, and themes.
alias sama="$HOME/org/scripts/org_sync.sh"
alias grind="$ORGDIR/future_vyapari/algos/leetcode_fetch.sh"

# Source fzf if installed
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# NVM (Node Version Manager) configuration
export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Powerlevel10k configuration
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Source Powerlevel10k theme
source /opt/homebrew/share/powerlevel10k/powerlevel10k.zsh-theme
export PATH="/usr/local/opt/postgresql@15/bin:$PATH"

export PATH="/opt/homebrew/opt/postgresql@15/bin:$PATH"
export PATH="/opt/homebrew/opt/postgresql@16/bin:$PATH"
