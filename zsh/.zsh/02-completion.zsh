# more information:  https://github.com/zsh-users/zsh-completions/blob/master/zsh-completions-howto.org

zmodload zsh/complist
autoload -Uz compinit

# completion listings
# zstyle ':completion:*:default' list-prompt '%S%M matches%s'
# bindkey -M listscroll q send-break # TODO: more keybindings for a more vim-like experience?


if [[ -n ${ZDOTDIR}/.zcompdump(#qN.mh+24) ]]; then
	compinit;
else
	compinit -C;
fi;

# no / and * and the like after file and dirnames
unsetopt list_types

# let dotfiles and dirs be matched w/o explicitly spec dot
# setopt globdots

# in forms like var=value, have things completed on value part
setopt magic_equal_subst

# on ambiguity, only fill in the characters that are common across a set of
# matches, do not complete completely
setopt list_ambiguous

# turn off beep / bell on ambiguous completion
unsetopt list_beep

# do not descend into completion menu via tab, ever. Just show stuff.
# setopt noautomenu

# when completion is ambiguous, do not generate list
unsetopt autolist

# layout for menu select:
# :completion:FUNCTION:COMPLETER:COMMAND-OR-MAGIC-CONTEXT:ARGUMENT:TAG

# menu based matching, starting at 0 matches
# zstyle ':completion:*' menu 'select=0'

# completers to use
zstyle ':completion:*' completer _expand _complete _ignored _approximate _match #TODO: what is match?

# (case insensitive matching) and (matching on both sides of the current word)
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'
# zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=* m:{a-zA-Z}={A-Za-z}'

# show completion types
zstyle ':completion:*:descriptions' format %B%U$'\e[38;5;111m%d'%b%u

# group completions under types
zstyle ':completion:*' group-name ''

# manpage sections
zstyle ':completion:*:manuals' separate-sections true
# zstyle ':completion:*:manuals.(^1*)' insert-sections true

# show command argument descriptions
zstyle ':completion:*' verbose yes
zstyle ':completion:*' list-separator '#'     # TODO:  different color, how?
zstyle ':completion:*' auto-description '%d'

# completion speed / caching
zstyle ':completion::complete:*' use-cache on               # completion caching, use rehash to clear
zstyle ':completion:*' cache-path $ZDOTDIR/cache              # cache path

# TODO:  what does match do?
zstyle ':completion:*:match:*' original only

zstyle ':completion:*:approximate:*' max-errors 1 numeric

# increase number of erros based on the length of the typed word
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'

# array completion element sorting
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters


# Directories
# zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
zstyle ':completion:*' squeeze-slashes true

# History
zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes

# Environmental Variables
zstyle ':completion::*:(-command-|export):*' fake-parameters ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}

# Populate hostname completion.
# TODO:  what does this do, exactly?
# zstyle -e ':completion:*:hosts' hosts 'reply=(
#   ${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) 2>/dev/null)"}%%[#| ]*}//,/ }
#   ${=${(f)"$(cat /etc/hosts(|)(N) <<(ypcat hosts 2>/dev/null))"}%%\#*}
#   ${=${${${${(@M)${(f)"$(cat ~/.ssh/config 2>/dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
# )'
zstyle -e ':completion:*:hosts' hosts 'reply=(
  ${=${${${${(@M)${(f)"$(cat ~/.ssh/config 2>/dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
)' #TODO: how to make zsh fill in the default user for the host as specced in .ssh/config?

# SSH/SCP/RSYNC # TODO understand?
zstyle ':completion:*:(scp|rsync):*' tag-order 'hosts:-host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:(scp|rsync):*' group-order users files all-files hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order users 'hosts:-host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:ssh:*' group-order hosts-domain hosts-host users hosts-ipaddr
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*.*' loopback localhost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-domain' ignored-patterns '<->.<->.<->.<->' '^*.*' '*@*'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^<->.<->.<->.<->' '127.0.0.<->'

# kill
zstyle ':completion:*:processes' command 'ps -au$USER' # TODO:  add color (see kill command below)
# zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,comm -w'
# zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single
zstyle ':completion:*:*:kill:*' list-colors '=(#b) #([0-9]#)*( *[a-z])*=34=31=33'
# zstyle ':completion:*:processes' list-colors '=(#b) #([0-9]#)*( *[a-z])*=34=31=33'

# Media Players # TODO understand
# zstyle ':completion:*:*:mpg123:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
# zstyle ':completion:*:*:mpg321:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
# zstyle ':completion:*:*:ogg123:*' file-patterns '*.(ogg|OGG|flac):ogg\ files *(-/):directories'
# zstyle ':completion:*:*:mocp:*' file-patterns '*.(wav|WAV|mp3|MP3|ogg|OGG|flac):ogg\ files *(-/):directories'

#generic completion with --help
compdef _gnu_generic gcc
compdef _gnu_generic gdb

#  ** colors
# zstyle list-colors ${(s.:.)LS_COLORS}

#  *** completion categories
zstyle ':completion:*:parameters' list-colors '=*=32'
zstyle ':completion:*:cursors' list-colors '=*=32'
zstyle ':completion:*:cursor' list-colors '=*=32'
zstyle ':completion:*:commands' list-colors '=*=1;38;5;2'
zstyle ':completion:*:builtins' list-colors '=*=31'
zstyle ':completion:*:alias' list-colors '=*=31'
# zstyle ':completion:*:options' list-colors '=^(* # )=35'
# zstyle ':completion:*:options' list-colors '=( #* )=31'

#  *** menu select
zstyle ':completion:*' list-colors 'ma=48;5;1;38;5;0'  # selection cursor
zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==38;5;12=33}:${(s.:.)LS_COLORS}")'
# zstyle ':completion:*:default' list-colors 'ma=32'


#  *** reference
# zstyle ':completion:*:*:*:*:*' menu select
# zstyle ':completion:*:matches' group 'yes'
# zstyle ':completion:*:options' description 'yes'
# zstyle ':completion:*:options' auto-description '%d'
# zstyle ':completion:*:corrections' format ' %F{green}-- %d (errors: %e) --%f'
# zstyle ':completion:*:descriptions' format ' %F{yellow}-- %d --%f'
# zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
# zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
# zstyle ':completion:*:default' list-prompt '%S%M matches%s'
# zstyle ':completion:*' format ' %F{yellow}-- %d --%f'
# zstyle ':completion:*' group-name ''
# zstyle ':completion:*' verbose yes


# use pager to navigate completion candidates
# zstyle ':completion:*:default' list-prompt '%S%M matches%s'  # TODO format?

# TODO:  difference of listscroll keymap and menuselect?

bindkey -M menuselect '\C-o' accept-and-menu-complete # TODO:  accept-line-and-down-history?

# delete style in specific context
# zstyle -d ':completion:windows' menu


