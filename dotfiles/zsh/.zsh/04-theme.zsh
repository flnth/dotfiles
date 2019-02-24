

# ZSH_HIGHLIGHT_HIGHLIGHTERS+=(main brackets)
# source $HOME/.zfunctions/syntax-highlighting/zsh-syntax-highlighting.zsh
# # Change comment color for Solarized color palette
# ZSH_HIGHLIGHT_STYLES[comment]='fg=green,bold'
# zle_highlight+=(paste:none)

export HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND=bg=105,fg=233

#  * shell prompt
export GEOMETRY_COLOR_ROOT="brightgreen"
export GEOMETRY_COLOR_PROMPT="brightcyan"
export GEOMETRY_COLOR_GIT_CONFLICTS_UNSOLVED="brightred"
export GEOMETRY_COLOR_GIT_CONFLICTS_SOLVED="brightgreen"
export GEOMETRY_COLOR_DIR="yellow"

#  * less, ls, etc.
# less syntax coloring
LESSOPEN="|lesspipe.sh %s"; export LESSOPEN

LS_COLORS=$LS_COLORS:'di=3;92:fi=0;0:ln=0,35:ex=0;94:mi=1;91:so=92:bd=92:cd=92:su=92:sg=0:tw=1:ow=1'
export LS_COLORS

# ZLS_COLORS=$LS_COLORS;
# export ZLS_COLORS;

# -- dircolors output (!!)
# LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:';
# export LS_COLORS


# syntax highlighting
typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)
# ZSH_HIGHLIGHT_STYLES[builtin]='none'
ZSH_HIGHLIGHT_STYLES[alias]=fg='10'
# ZSH_HIGHLIGHT_STYLES[function]=fg='200'
# ZSH_HIGHLIGHT_STYLES[command]=fg='200'
# ZSH_HIGHLIGHT_STYLES[precommand]='color107'
# ZSH_HIGHLIGHT_STYLES[hashed-command]='none'
# ZSH_HIGHLIGHT_STYLES[default]=fg='color129'
# ZSH_HIGHLIGHT_STYLES[unknown-token]=fg='color129'
ZSH_HIGHLIGHT_STYLES[path]=fg='12'
# ZSH_HIGHLIGHT_STYLES[suffix-alias]=fg='yellow,bold'
# more: https://blog.patshead.com/2012/01/using-and-customizing-zsh-syntax-highlighting-with-oh-my-zsh.html

# zle_highlight=(region:bg=235 isearch:fg=9 suffix:fg=12 special:fg=9 paste:fg=13 )

# dircolors.moonshine
eval `dircolors ls.colors`

#  * man-pages and less
export LESS_TERMCAP_mb=$(printf '\e[01;31m') # enter blinking mode
export LESS_TERMCAP_md=$(printf '\033[38;2;255;175;95m') # enter double-bright mode
export LESS_TERMCAP_me=$(printf '\e[0m') # turn off all appearance modes (mb, md, so, us)
export LESS_TERMCAP_se=$(printf '\e[0m') # leave standout mode
export LESS_TERMCAP_so=$(printf '\e[03;34;1m') # enter standout mode  (status line)
export LESS_TERMCAP_ue=$(printf '\e[0m') # leave underline mode
export LESS_TERMCAP_us=$(printf '\033[04;35m') # enter underline mode
