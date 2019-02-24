
# █▓▒░ history

HISTFILE=~/.zsh_history
HISTSIZE=5000
SAVEHIST=5000

setopt appendhistory
setopt inc_append_history  # add commands incrementally, as they are typed
setopt share_history       # share hist between sessions
setopt hist_reduce_blanks  # trim blanks
setopt bang_hist           # !keyword
# setopt ignoreeof

