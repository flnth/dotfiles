
#█▓▒░ utils:   functions and aliases
cur_dir=$(dirname $(readlink -f "$0"))
#for alias in $cur_dir/alias/*.zsh; do source $cur_dir/alias/$alias ; done

#for f in $cur_dir/alias/*.zsh; do
#	source $f
#done



#system_dep_utils="utils/zsh.$(hostname).utils"
#[[ -f $system_dep_utils]] && source $system_dep_utils

#source $cur_dir/fasd.zsh
#source $cur_dir/fzf-key-bindings.zsh

# load host-specific aliases
#if [[ -f "alias/alias.$(uname -n).zsh" ]]; then
#	source "utils/alias.$(uname -n).zsh"
#fi

# ----------------------------------------------------------
# TODO:  sort below into alias / functions

#rsync
alias rsync='rsync -rzvvhP'

# zeal
alias zeal="$DIR_LOCAL/bin/zeal"

alias logout="kill -9 -1"


# opening files with superuser privileges using emacs
sudo_emacs()
{
	if [ $1 = "ee" ]; then
		fpath=`pwd`/$2
		$DIR_LOCAL/bin/emacsclient -nw --socket-name=/tmp/emacs1000/server -e "(find-file \"/sudo::$fpath\")"
	else
		sudo "$@"
	fi
}
alias sudo=sudo_emacs

# silver searcher ag
alias ag='ag --pager "less -R" --smart-case'

# directory tree listing
alias lst='ls -R | grep ":$" | sed -e '"'"'s/:$//'"'"' -e '"'"'s/[^-][^\/]*\//--/g'"'"' -e '"'"'s/^/   /'"'"' -e '"'"'s/-/|/'"'"

# vim less emulator for coloured file display in terminal
alias lessc='/usr/share/vim/vim80/macros/less.sh'

# coloured df:  sudo apt install dfc (!!)

# directories only
alias lsd="ls */ -d"

# copy / paste stuff
alias pbcopy="xsel --clipboard --input"
alias pbpaste="xsel --clipboard --output"

# clipboard
alias c="xclip -i -selection clipboard"
function c.(){
    echo "$(pwd -P)" | tr -d '\n' | sed "s/\s/\\\ /g" | xclip -i -selection clipboard
}

# TODO:  colorize output  via escape codes, like this:    ll|awk '{print " \033[0;34m " $8 " \033[0;32m " $5 " \033[0;31m " $3}'
alias ls="ls --color=tty --group-directories-first"

alias less="less -R"

ll()
{
	# default configuration:  alphabetic sort, show everything, top down  (for an overview)
	# non-default via passing arguments

	# non-default configuration:
	# echo "" | python -c 'import pty, sys; pty.spawn(sys.argv[1:])' ls++ "$@" | less -R -j5
	# echo "" | python -c 'import pty, sys; pty.spawn(sys.argv[1:])' ls++ "$@" | fzf --ansi
	echo "" | python -c 'import pty, sys; pty.spawn(sys.argv[1:])' ls++
}

llt()
{
	# default configuration: sort by time modified, oldest first, bottom up
	echo "" | python -c 'import pty, sys; pty.spawn(sys.argv[1:])' ls++ -tr | less -R -j5 +G
}



# alias ls="~/software/ls--/ls++"

alias lsd="ls */ -d"

# alias lt="l -t | awk '{print \" \033[0;34m \" $3 $4}' "
# function lt(){
#     l -t | awk '{print $1 "  " $2 " \x1b[38;5;12m" $3 " " $4 " " "\x1b[38;5;246m" $5 $6 $7 $8 $9 }'
# }


alias pdb="$DIR_ANACONDA/lib/python3.5/pdb.py"
alias pytest="python -m pytest"

# tmux

tmux_show_text()
{
	tmux setw -g status-right $newtext
}

alias tmux='$DIR_LOCAL/bin/tmux'
alias tmuxn='tmux new-session -s $$'
_trap_exit() { tmux kill-session -t $$ }
trap _trap_exit EXIT

# todo: define this at home, only, e.g. by checking against ... hostname?
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME $@'

# repository syncing
alias reposync="python3 $DIR_SYSTEM/reposync.py"
alias orgsync="python3 $DIR_SYSTEM/orgsync.py"

# got
alias got='ps awx | grep'

# # fzf browser
# source ~/.fzf_browser/fzf_browser.sh
# source ~/.fzf_browser/zsh_widgets.zsh
# bindkey "^b" _fuzzybrowse_zsh_insert_output



# alias urxvt='$DIR_LOCAL/bin/urxvt'
# alias urxvt='$DIR_LOCAL/bin/urxvt -pe kuake'          # ... starting tmux from
                                                      # the beginning does weird
                                                      # things, aparently

#  * emacs
#alias emacs='$DIR_LOCAL/bin/emacs'
# alias _emacsalias='$DIR_LOCAL/bin/emacsclient -nw --socket-name=/tmp/emacs1000/server'          # opens emacs right here
#alias ee='emacsclient -nw'
alias e='$DIR_LOCAL/bin/emacsclient --no-wait --socket-name=/tmp/emacs1000/server'     # opens file in emacs
alias eg='$DIR_LOCAL/bin/emacsclient -c --socket-name=/tmp/emacs1000/server'          # open new GUI frame

function _call_emacs()
{
	$DIR_LOCAL/bin/emacsclient -c -nw --socket-name=/tmp/emacs1000/server $@
}

function ee
{
    # If the argument is - then write stdin to a tempfile and open the
    # tempfile.
    if [[ $# -ge 1 ]] && [[ "$1" == - ]]; then
        tempfile="$(mktemp emacs-stdin-$USER.XXXXXXX --tmpdir)"
        cat - > "$tempfile"
        _call_emacs --eval "(find-file \"$tempfile\")" \
            --eval '(set-visited-file-name nil)' \
            --eval '(rename-buffer "*stdin*" t))'
    else
        _call_emacs "$@"
    fi
}


cde() {
	cd ${(Q)~$($DIR_LOCAL/bin/emacsclient -n --socket-name=/tmp/emacs1000/server -e '(with-current-buffer
							   (window-buffer (selected-window))
							   default-directory) ')}
}

# alias elfeed='ee -e "(fn-elfeed-from-term)"'

function elfeed() {
    ee -e "(fn-elfeed-from-term)"
}



#  * dev

# apt-get install

alias qtcreator="LD_PRELOAD=$DIR_SYSTEM/qt5noblink.so $DIR_QTCREATOR/bin/qtcreator"
alias pycharm="$DIR_PYCHARM/pycharm.sh"

#  ** coloring
ccat()
{
    if [ -t 1 ]; then
        source-highlight --failsafe -n --infer-lang -f esc --style-file="$DIR_SYSTEM/terminal/default.style" -i $@
    else
        cat $@;
    fi
}
alias cat=ccat

chead()
{
	local input;
	if [ ! -t 0 ]; then
		input=$(cat)
		echo $input | /usr/bin/head
	else
		source-highlight --line-range="0-10" -n --failsafe --infer-lang -f esc --style-file="$DIR_SYSTEM/terminal/default.style" -i $@$input
	fi
}
alias head=chead



#  ** pagers / viewers

alias man='COLUMNS=80 man --pager="less -j5 -z-20 +Gg"'

#  * utils
#  ** time

# #
# function dates(){
#     TZ='UTC'

# }

#  ** emacs
# alias magit='emacsclient -nw -e "(spacemacs/switch-to-scratch-buffer)" "(magit-status-prompt)"'
magit-status(){
	zle reset-prompt
    ee -e "(fn-magit-status '($*))"
	zle reset-prompt
}
zle -N magit-status

magit-log(){
	zle reset-prompt
	ee -e "(fn-magit-log)"
	zle reset-prompt
}
zle -N magit-log

newmail(){
    emacsclient -nw -e "(notmuch-mua-mail)"
}

#  ** mail reading

# - if there's only one mail, do not show fzf, but show it directly
# - as soon as a message has been read, remove its unread tag
# - provide interface for
#   . adding additional tags (necessary?)
#   . opening the message in emacs (in thread view for context)
#   . replying to all senders in emacs

notmuch-unread(){
    notmuch search tag:unread | cut -d' ' -f 3- |
        fzf --ansi --no-sort --reverse --tiebreak=index \
            --bind "ctrl-m:execute:
            (echo {} ) << FZF-EOF"
}


# notmuch address thread:000000000432   -> print address
# notmuch

# function fshow() {
#     git log --graph --color=always \
#         --format="%Cred %C(auto)%h%d %<(40) %s %C(32) %an %C(#98bd5e) %cr" "$@" $1 |
#         fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
#             --bind "ctrl-m:execute:
#                 (grep -o '[a-f0-9]\{7\}' | head -1 |
#                 xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
#                 {}
# FZF-EOF" \
#             --bind "ctrl-d:execute:
#                 (grep -o '[a-f0-9]\{7\}' | head -1 |
#                 xargs -I % sh -c 'git diff --color=always % | less -R') << 'FZF-EOF'
#                 {} FZF-EOF" \
#                     --bind "ctrl-c:execute:
#                 (grep -o '[a-f0-9]\{7\}' | head -1 |
#                 xargs -I % sh -c 'git checkout %') << 'FZF-EOF'
#                 {} FZF-EOF"
# }

notmuch-print-mail(){
    # input:  a full line of notmuch search
    # extracts the threadid from it, parses notmuch show output and
    # prints the header and message (text) body

    thread=`echo $1 | awk {'print $1'}`
    echo $thread
    msg=`notmuch show $thread`

    linebreaks=`echo $msg | grep $'^\f' -n`

    # header
    header_text=`echo $linebreaks | grep header | cut -d':' -f1`
    hstart=$((`echo $header_text | head -n 1` + 1))
    hend=$((`echo $header_text | tail -n 1` - 1))
    header=`echo $msg | sed -n "$hstart,$hend p"`

    # message body
    body_start=$((`echo $linebreaks | grep text/plain | cut -d':' -f1` + 1))
    l=$((`echo $linebreaks | grep text/plain -n | cut -d':' -f1` + 1))
    body_end=$((`echo $linebreaks | sed -n "$l,$l p" | cut -d':' -f1` - 1))
    body=`echo $msg | sed -n "$body_start,$body_end p"`

    # do some syntax coloring?
    echo $header
    echo $body
}

#  ** mlocate

tilde-to-home()
{
	echo $1 | sed s@~@"$HOME"@
}

home-to-tilde()
{
	echo $1 | sed s@"$HOME"@~@
}

loc ()
{
	[[ -z $MLOCATEDBS ]] && return
	locate -LAi --database $MLOCATEDBS "" $@ | sed s@"$HOME"@~@
}

fzf-ff()
{
	local key sel out
	out=$( { loc $@ ; fasd -fl } | fzf1 --expect=enter,alt-d )
	# out=(${=out})
	out=("${(f)out}")
	key=$out[1]
    # sel=("${(@s/:/)out[2]}")  # get filename only, or something?

	sel=$( tilde-to-home $out[2] )

	[ -n $sel ] && case $key in
		enter) open-file-or-cd-dir $sel;;
		alt-d) enter-dir-of $sel ;;
	esac
	zle reset-prompt
	zle-line-init
}
zle -N fzf-ff

fzf-gb()
{
	local key sel out
	out=$( { fasd -flt } | fzf1 --tac --expect=enter,alt-d )
	# out=(${=out})
	out=("${(f)out}")
	key=$out[1]
	sel=("${(@s/:/)out[2]}")

	[ -n $sel ] && case $key in
		enter) open-file-or-cd-dir $sel;;
		alt-d) enter-dir-of $sel[1] ;;
	esac
	zle reset-prompt
	zle-line-init
}
zle -N fzf-gb

# check out git branch/tag, ordered by recency
# TODO: with an optional preview if there's room
fzf-gC()
{
  local branches out key branch
  branches=$(git for-each-ref --sort=-committerdate refs/heads/ --format="%(refname:short)")
  # TODO: formatting
  # git for-each-ref --sort=committerdate refs/heads/ --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject) - %(authorname) (%(color:green)%(committerdate:relative)%(color:reset))'
  out=$(echo "$branches" |
           fzf1 -d $(( 2 + $(wc -l <<< "$branches") )) +m --expect=enter)
  out=("${(f)out}")
  key=$out[1]
  branch=("${(@s/:/)out[2]}")

  [ -n $sel ] && case $key in
	  enter) git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##") &&
				     zle-line-init && zle accept-line && return;;
  esac
  zle reset-prompt
}
zle -N fzf-gC

# like fzf-gC, but also shows remote branches
fzf-ugC()
{
  local branches out key branch
  branches=$(git branch --all --sort=-committerdate)
  out=$(echo "$branches" |
           fzf1 -d $(( 2 + $(wc -l <<< "$branches") )) +m --expect=enter)
  out=("${(f)out}")
  key=$out[1]
  branch=("${(@s/:/)out[2]}")

  [ -n $sel ] && case $key in
	  enter) git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##") &&
				     zle-line-init && zle accept-line && return;;
  esac
  zle reset-prompt

}
zle -N fzf-ugC

#  ** synchronizing / archiving
alias archive="$DIR_SYSTEM/archive.py"
alias reposync="$DIR_SYSTEM/reposync.py"

#  ** media

# conversion to mp3  (play with cmus)
convmp3(){
    ffmpeg -i $1 -c:a libmp3lame -ac 2 -b:a 68k $1.mp3
}

# Image optimization / conversion / processing
png() {
  pngcrush -brute "$1"{,.} && du -b "$1"{,.}
}

gif() {
  gifsicle -O "$1" -o "$1." && du -b "$1"{,.}
}

invert() {
    convert "$1" -negate "$2"
}

# alias jpegtran="$STACKROOT/software/jpegcrop/jpegtran"
# jpeg()
# {
#   jpegtran "$1" > "$1." && du -b "$1"{,.}
# }

# pdfsizeopt tool (TODO:  execute this only at home...)
export PATH="/home/fthevissen/software/pdfsizeopt:$PATH"


#  ** looking up information
#todo:  pass in initial arguments to ag and/or fzf
function checknotes(){
    notesdir=$(echo $DIR_NOTES | sed -e 's/\\//g')
    ret="`cd $notesdir; ag --nobreak --noheading . | fzf`"
    if [ ! -z "$ret" ]
    then
        file=$(echo $ret | cut -d ":" -f1)
        linenum=$(echo $ret | cut -d ":" -f2)
        $DIR_LOCAL/bin/emacsclient +$linenum "$notesdir/$file" -nw
    fi
}


#  ** fzf

# https://github.com/junegunn/fzf/wiki/Color-schemes
export FZF_DEFAULT_OPTS="
--color fg:-1,bg:-1,hl:11,hl+:232,fg+:233,bg+:1
--color info:238,prompt:238,spinner:150,pointer:238,marker:174 --inline-info"

#  *** git utils
# GIT heart FZF
# -------------

is_in_git_repo() {
  git rev-parse HEAD > /dev/null 2>&1
}

fzf-down() {
  fzf --height 50% "$@" --border
}

open-file-or-cd-dir(){
	[[ -d "$1" ]] && cd "$1" \
			    ||  o "$1"
}

enter-dir-of(){
	if [[ -d "$1" ]]; then
		cd "$1"
	elif [[ -f "$1" ]]; then
		cd "$(dirname "$1")"
	fi
}

fzf1() {
	fzf --height 20% --reverse --prompt='' --no-bold \
		--bind='enter:execute(echo {})' \
		--bind="ctrl-d:half-page-down" \
		--bind="ctrl-u:half-page-up" \
		"$@"
}

fzf-pf() {
	# if in git repo, search from root down, otherwise from pwd.
	# keys:
	#   - left-right:  switch between files/directories
	#   - enter:       open file or directory
	#   - C-d:         open directory (of file
	#   - C-o:         TODO: insert (multi-)selection to commandline

	# if in git repo, ask for query prompt, pass to (ripgrep | fzf)
	local cpwd root out
	cpwd=$(pwd)
	is_in_git_repo && root=$(git rev-parse --show-toplevel) \
			|| root=$cpwd
	cd $root

	zle reset-prompt
	zle-line-init

	out=$(rg --files ./ 2> /dev/null | fzf1 --ansi --expect=enter,alt-d --color hl:2)
	# out=(${=out})
	out=("${(f)out}")
	key=$out[1]
	sel=$out[2]

	[ -n $sel ] && case $key in
		enter)
			open-file-or-cd-dir $sel;
			cd $cpwd;;
		alt-d) enter-dir-of $sel ;;
	esac

	zle reset-prompt
	zle-line-init
}
zle -N fzf-pf

fzf-Mf() {
	out=$(rg --files ./ 2> /dev/null | fzf1 --ansi --expect=enter,alt-d --color hl:2)
	out=("${(f)out}")
	key=$out[1]
	sel=$out[2]

	[ -n $sel ] && case $key in
		enter) open-file-or-cd-dir $sel;;
		alt-d) enter-dir-of $sel ;;
	esac

	zle reset-prompt
	zle-line-init
}
zle -N fzf-Mf

autoload -U read-from-minibuffer

FZF_PS_PREVIEW_COMMAND='
  local __file="$(echo {} | awk -F ":" '\''{print $1}'\'')"
  local __line_nr="$(echo {} | awk -F ":" '\''{print $2}'\'')"

  local __start_line=$(( __line_nr - __PREVIEW_HALF_HEIGHT ))
  local __end_line=$(( __line_nr +  __PREVIEW_HALF_HEIGHT ))

  local __style_normal=zenburn
  local __style_line=zenburn

  __hl_content=$((highlight -O ansi -l --force "$__file" --line-number-start=$__start_line --line-range=$__start_line-$__end_line -c __style_normal  ) 2> /dev/null)
  local __hl_line=$( highlight -O ansi -l --force --line-number-start=$__line_nr --line-range=$__line_nr-$__line_nr $__file | rg --color=always --colors "match:bg:yellow" --colors "match:fg:black" --colors "match:style:intense" "__PATTERN" )
  echo $__hl_content | sed -n "1,__PREVIEW_HALF_HEIGHTp";
  echo $__hl_line;
  echo $__hl_content | sed -n "$((__PREVIEW_HALF_HEIGHT+2)),$ p";
'

fzf-ps()
{
	# if in git repo, ask for query prompt, pass to (ripgrep | fzf)
	local cpwd root out
	cpwd=$(pwd)
	is_in_git_repo && root=$(git rev-parse --show-toplevel) \
			|| root=$cpwd
	cd $root

	local __prev_window=down
	local __prev_half_height=$(( $(tput lines) / 4 -1 ))
	[[ $COLUMNS > 170  ]] && \
		__prev_window=right && \
		__prev_half_height=$(( $(tput lines) / 2 -1 ))

	read-from-minibuffer "> "
	local __pattern=$REPLY
	local __escaped_pattern=$(echo "$__pattern" | sed -e 's/[\/&]/\\&/g')
	local __preview_cmd=$(echo "$FZF_PS_PREVIEW_COMMAND" | sed "s/__PATTERN/$__escaped_pattern/g" \
														 | sed "s/__PREVIEW_HALF_HEIGHT/$__prev_half_height/g"
		  )
	# echo "escaped_pattern: $__escaped_pattern"
	local __rg_ignore_file=$DIR_SYSTEM/rg_ignore

	out=($(rg -n --column --no-heading --color always --ignore-file $__rg_ignore_file $__pattern ./ 2> /dev/null | fzf1 --height 0% --ansi --expect=enter,alt-d --preview="$__preview_cmd" --preview-window=$__prev_window --color bg+:238,fg+:254))
	out=(${=out})
	key=$out[1]
    sel=("${(@s/:/)out[2]}")

	[ -n $sel[1] ] && case $key in
		enter)
			ee -e "(find-file-flash-line \"$sel[1]\" $sel[2] $sel[3])" ;
			cd $cpwd;;
		alt-d) enter-dir-of $sel[1] ;;
	esac

	zle reset-prompt
	zle-line-init
}
zle -N fzf-ps
bindkey -M vicmd ' ps' fzf-ps

fzf-pS()
{
	local out
	local __prev_window=down
	local __prev_half_height=$(( $(tput lines) / 4 -1 ))
	[[ $COLUMNS > 170  ]] && \
		__prev_window=right && \
		__prev_half_height=$(( $(tput lines) / 2 -1 ))

	read-from-minibuffer "> "
	local __pattern=$REPLY
	local __escaped_pattern=$(echo "$__pattern" | sed -e 's/[\/&]/\\&/g')
	local __preview_cmd=$(echo "$FZF_PS_PREVIEW_COMMAND" | sed "s/__PATTERN/$__escaped_pattern/g" \
														 | sed "s/__PREVIEW_HALF_HEIGHT/$__prev_half_height/g"
		  )
	# echo "escaped_pattern: $__escaped_pattern"
	local __rg_ignore_file=$DIR_SYSTEM/rg_ignore

	out=($(rg -n --column --no-heading --color always --ignore-file $__rg_ignore_file $__pattern ./ 2> /dev/null | fzf1 --height 0% --ansi --expect=enter,alt-d --preview="$__preview_cmd" --preview-window=$__prev_window --color bg+:238,fg+:254))
	out=(${=out})
	key=$out[1]
    sel=("${(@s/:/)out[2]}")

	[ -n $sel[1] ] && case $key in
		enter)
			ee -e "(find-file-flash-line \"$sel[1]\" $sel[2] $sel[3])" ;
			cd $cpwd;;
		alt-d) enter-dir-of $sel[1] ;;
	esac

	zle reset-prompt
	zle-line-init
}
zle -N fzf-pS
bindkey -M vicmd ' pS' fzf-pS

fzf-ad()
{
	ee -e "(fn-dired $PWD t)"
}
zle -N fzf-ad
bindkey -M vicmd ' ad' fzf-ad

fasd-zd(){
	read-from-minibuffer "> "
	fasd -d $REPLY
}
zle -N fasd-zd

fasd-zf(){
	read-from-minibuffer "> "
	ee $(fasd -f -1 $REPLY)
}
zle -N fasd-zf

# TODO:  call function in emacs, passing the query into it,
#        then open magit status in there is possible
# fasd-zp(){
# 	read-from-minibuffer "> "
# }

gf() {
  is_in_git_repo || return
  git -c color.status=always status --short |
  fzf-down -m --ansi --nth 2..,.. \
    --preview '(git diff --color=always -- {-1} | sed 1,4d; cat {-1}) | head -500' |
  cut -c4- | sed 's/.* -> //'
}

gb() {
  is_in_git_repo || return
  git branch -a --color=always | grep -v '/HEAD\s' | sort |
  fzf-down --ansi --multi --tac --preview-window right:70% \
    --preview 'git log --oneline --graph --date=short --pretty="format:%C(auto)%cd %h%d %s" $(sed s/^..// <<< {} | cut -d" " -f1) | head -'$LINES |
  sed 's/^..//' | cut -d' ' -f1 |
  sed 's#^remotes/##'
}

gt() {
  is_in_git_repo || return
  git tag --sort -version:refname |
  fzf-down --multi --preview-window right:70% \
    --preview 'git show --color=always {} | head -'$LINES
}

gh() {
  is_in_git_repo || return
  git log --date=short --format="%C(green)%C(bold)%cd %C(auto)%h%d %s (%an)" --graph --color=always |
  fzf-down --ansi --no-sort --reverse --multi --bind 'ctrl-s:toggle-sort' \
    --header 'Press CTRL-S to toggle sort' \
    --preview 'grep -o "[a-f0-9]\{7,\}" <<< {} | xargs git show --color=always | head -'$LINES |
  grep -o "[a-f0-9]\{7,\}"
}

gr() {
  is_in_git_repo || return
  git remote -v | awk '{print $1 "\t" $2}' | uniq |
  fzf-down --tac \
    --preview 'git log --oneline --graph --date=short --pretty="format:%C(auto)%cd %h%d %s" {1} | head -200' |
  cut -d$'\t' -f1
}

gstash() {
  local out k reflog
  out=(
    $(git stash list --pretty='%C(yellow)%gd %>(14)%Cgreen%cr %C(blue)%gs' |
      fzf --ansi --no-sort --header='enter:show, ctrl-d:diff, ctrl-o:pop, ctrl-y:apply, ctrl-x:drop' \
          --preview='git stash show --color=always -p $(cut -d" " -f1 <<< {}) | head -'$LINES \
          --preview-window=down:50% --reverse \
          --bind='enter:execute(git stash show --color=always -p $(cut -d" " -f1 <<< {}) | less -r > /dev/tty)' \
          --bind='ctrl-d:execute(git dsf --color=always $(cut -d" " -f1 <<< {}) | less -r > /dev/tty)' \
          --expect=ctrl-o,ctrl-y,ctrl-x))
  k=${out[0]}
  reflog=${out[1]}
  [ -n "$reflog" ] && case "$k" in
    ctrl-o) git stash pop $reflog ;;
    ctrl-y) git stash apply $reflog ;;
    ctrl-x) git stash drop $reflog ;;
  esac
}

#  *** fag
# FZF_DEFAULT_OPTS="--extended-exact --height 100% --cycle --no-reverse --bind ctrl-a:select-all "
# FZF_DEFAULT_OPTS+="--bind pgup:preview-up --bind pgdn:preview-down --bind ctrl-f:jump --bind ctrl-k:kill-line --bind ctrl-p:toggle-preview"
# FZF_DEFAULT_PREVIEW_COMMAND='\
#   __file="$(echo {})"; \
#   [[ $(file -L "$__file") =~ directory ]] && ls -la --color=always "$__file" \
#   || [[ $(file -L --mime "$__file") =~ binary ]] && file -L "$__file" | cut -d":" -f2- \
#   || (highlight -O ansi -l "$__file" || coderay "$__file" || rougify "$__file" || cat "$__file") 2> /dev/null | head -500'

# export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview' --bind 'ctrl-y:execute-silent(echo -n {2..} | pbcopy)+abort' --header 'Press CTRL-Y to copy command into clipboard' --border"

# fzf() {
#   local __PREVIEW_WINDOW_COMMAND=$(if [ `tput cols` -lt 140 ]; then echo "up"; else echo "right"; fi)
#   fzf --preview-window="$__PREVIEW_WINDOW_COMMAND" --preview="$FZF_DEFAULT_PREVIEW_COMMAND" $FZF_DEFAULT_OPTS "$@" 
# }

FZF_AG_PREVIEW_COMMAND='
  __file="$(echo {} | awk -F ":" '\''{print $1}'\'')"
  __line_nr="$(echo {} | awk -F ":" '\''{print $2}'\'')"
  __hl_content=$((highlight -O ansi -l --force "$__file" || coderay "$__file" || rougify "$__file" || cat "$__file") 2> /dev/null)
  __context_count=$(( $(tput lines) / 4 - 1 ))
  __start_line=$(( __line_nr - __context_count ))
  if (( __start_line < 1 )); then (( __start_line=1 )); fi
  if (( __start_line == __line_nr )); then (( __start_line=10000000000 )); fi
  __hl_line=$(printf "\e[1;33m%5s\e[0m " "$__line_nr"; cat "$__file" | sed "${__line_nr}q;d" | ag --color '\''__PATTERN'\'' | tr -d "\n")
  echo "$__hl_content" | sed -n "${__start_line},$(( __line_nr-1 ))p"; echo "$__hl_line"; echo "$__hl_content" | sed -n "$(( __line_nr+1 )),$ p"'


fag() {
  local __args_backup=("$@")
  local __pattern=''
  local __ag_color='--color'
  while [ $# -gt 0 ]; do
    if [[ $1 =~ ^-.* ]]; then
      shift
    else
      __pattern="$1"
      break
    fi
  done
  set -- "${__args_backup[@]}"

  if [ "$__pattern" = "." ]; then
    __ag_color='--nocolor'
  fi

  local __escaped_pattern=$(echo "$__pattern" | sed -e 's/[\/&]/\\&/g')
  local __preview_cmd=$(echo "$FZF_AG_PREVIEW_COMMAND" | sed "s/__PATTERN/$__escaped_pattern/g")
  # local __selected=$(ag --nobreak --noheading --depth -1 -f --hidden "$__ag_color" "$@" 2> /dev/null | fzf --multi --ansi --preview="$__preview_cmd" --preview-window=down --expect=ctrl-o,ctrl-e,ctrl-s,ctrl-b,ctrl-n,ctrl-l,ctrl-r)
  local __selected=$(rg -n --column --color always "$@" 2> /dev/null | fzf --multi --ansi --preview="$__preview_cmd" --preview-window=down --expect=ctrl-o,ctrl-e,ctrl-s,ctrl-b,ctrl-l,ctrl-r)
}

# zsh-interactive-cd plugin (TODO: need?)
#source $DIR_SYSTEM/terminal/zsh-interactive-cd/zsh-interactive-cd.plugin.zsh

#  *** fshow - git commit browser
# http://junegunn.kr/2015/03/browsing-git-commits-with-fzf/
function fshow() {
  git log --graph --color=always \
      --format="%Cred %C(auto)%h%d %<(40) %s %C(32) %an %C(#98bd5e) %cr" "$@" $1 |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF" \
      --bind "ctrl-d:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git diff --color=always % | less -R') << 'FZF-EOF'
                {} FZF-EOF" \
      --bind "ctrl-c:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git checkout %') << 'FZF-EOF'
                {} FZF-EOF"
}

#same for bookmarks:
# http://junegunn.kr/2015/04/browsing-chrome-bookmarks-with-fzf/
# more stuff here:
# https://www.youtube.com/playlist?list=PLqv94xWU9zZ2fMsMMDF4PjtNHCeBFbggD

#  *** fe
# fe [FUZZY PATTERN] - Open the selected file with the default editor
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-0)
fe() {
  IFS='
'
  local declare files=($(fzf-tmux --query="$1" --select-1 --exit-0))
  [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
  unset IFS
}

#  *** fo
# Modified version where you can press
#   - CTRL-O to open with `open` command,
#   - CTRL-E or Enter key to open with the $EDITOR
fo() {
  local out file key
  out=$(fzf-tmux --query="$1" --exit-0 --expect=ctrl-o,ctrl-e)
  key=$(head -1 <<< "$out")
  file=$(head -2 <<< "$out" | tail -1)
  if [ -n "$file" ]; then
    [ "$key" = ctrl-o ] && xdg-open "$file" || ${EDITOR:-vim} "$file"
  fi
}

#  *** vf
# ex: vf word1 word2 ... (even part of a file name)
# zsh autoload function
vf() {
  local files

  files=(${(f)"$(locate -Ai -0 $@ | grep -z -vE '~$' | fzf --read0 -0 -1 -m)"})

  if [[ -n $files ]]
  then
     vim -- $files
     print -l $files[1]
  fi
}

#  *** fd
# fd - cd to selected directory
# fd() {
# }

#  *** cf
# cf - fuzzy cd from anywhere
# ex: cf word1 word2 ... (even part of a file name)
# zsh autoload function
cf() {
  local file

  file="$(locate -Ai -0 $@ | grep -z -vE '~$' | fzf --read0 -0 -1)"

  if [[ -n $file ]]
  then
     if [[ -d $file ]]
     then
        cd -- $file
     else
        cd -- ${file:h}
     fi
  fi
}

#  *** cdf
# cdf - cd into the directory of the selected file
cdf() {
   local file
   local dir
   file=$(fzf +m -q "$1") && dir=$(dirname "$file") && cd "$dir"
}

#  *** fh
# fh - repeat history
fh() {
  print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
}

#  *** fbr
# fbr - checkout git branch
fbr() {
  local branches branch
  branches=$(git branch -vv) &&
  branch=$(echo "$branches" | fzf-tmux +m) &&
  git checkout $(echo "$branch" | awk '{print $1}' | sed "s/.* //")
}

# fbr - checkout git branch (including remote branches)
fbr() {
  local branches branch
  branches=$(git branch --all | grep -v HEAD) &&
  branch=$(echo "$branches" |
           fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
  git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}


#  *** fco
# fco - checkout git branch/tag
fco() {
  local tags branches target
  tags=$(git tag | awk '{print "\x1b[31;1mtag\x1b[m\t" $1}') || return
  branches=$(
    git branch --all | grep -v HEAD             |
    sed "s/.* //"    | sed "s#remotes/[^/]*/##" |
    sort -u          | awk '{print "\x1b[34;1mbranch\x1b[m\t" $1}') || return
  target=$(
    (echo "$tags"; echo "$branches") | sed '/^$/d' |
    fzf-down --no-hscroll --reverse --ansi +m -d "\t" -n 2 -q "$*") || return
  git checkout $(echo "$target" | awk '{print $2}')
}

#  *** fcoc
# fcoc - checkout git commit
fcoc() {
  local commits commit
  commits=$(git log --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | fzf --tac +s +m -e) &&
  git checkout $(echo "$commit" | sed "s/ .*//")
}

#  *** fcs
# fcs - get git commit sha
# example usage: git rebase -i `fcs`
fcs() {
  local commits commit
  commits=$(git log --color=always --pretty=oneline --abbrev-commit --reverse)
  commit=$( echo $commits | fzf --tac +s +m -e --ansi --reverse) &&
  echo -n $(echo "$commit" | sed "s/ .*//")
}


#  *** fstash
# fstash - easier way to deal with stashes
# type fstash to get a list of your stashes
# enter shows you the contents of the stash
# ctrl-d shows a diff of the stash against your current HEAD
# ctrl-b checks the stash out as a branch, for easier merging
fstash() {
  local out q k sha
  while out=$(
		  git stash list --pretty="%C(yellow)%h %>(14)%Cgreen%cr %C(blue)%gs" |
			  fzf --ansi --no-sort --query="$q" --print-query \
				  --expect=ctrl-d,ctrl-b);
  do
      q=$(head -1 <<< "$out")
      k=$(head -2 <<< "$out" | tail -1)
      sha=$(tail -1 <<< "$out" | cut -d' ' -f1)
      [ -z "$sha" ] && continue
      if [ "$k" = 'ctrl-d' ]; then
        git diff $sha
      elif [ "$k" = 'ctrl-b' ]; then
        git stash branch "stash-$sha" $sha
        break;
      else
        git stash show -p $sha
      fi
    done
}

#  *** dh
#  directory stack listing with fzf ---------
function dh(){
    chpwd
    TAB=$'\t'
    results="$(dirs -v)"
    for param in "$@"
    do
        results=$(echo $results | grep "$param")
    done
    directory="$(echo "$results" | fzf-tmux -1 -0 --no-sort +m | sed 's/ *[0-9]* *//' | sed 's/\t//g' | sed 's/ /\\ /g')"
    #cd "$directory"   # doesnt work
    eval "$directory"
}

# zle     -N   fzf-open-file-or-dir
# bindkey '^P' fzf-open-file-or-dir

#  *** fzf-open-file-or-dir
function fzf-open-file-or-dir(){
    if [ $# -eq 0 ]
    then
        spath="."
    else
        spath="$1"
    fi

    local cmd="command find -L '$spath' \
    \\( -path '*/\\.*' -o -fstype 'dev' -o -fstype 'proc' \\) -prune \
    -o -type f -print \
    -o -type d -print \
    -o -type l -print 2> /dev/null | sed 1d"
    local out="$(eval $cmd | fzf --exit-0 --reverse --height 20)"

    if [ -f "$out" ]; then
        o "$out"
    elif [ -d "$out" ]; then
        cd "$out"
    fi
}

#  *** fzf-open-frecently
function fzf-open-frecently(){
    local cmd=" fasd -l"
    local out="$(eval $cmd | fzf --exit-0 --reverse --height 20)"

    if [ -f "$out" ]; then
        xdg-open "$out"
    elif [ -d "$out" ]; then
        cd "$out"
    fi
}
zle -N        fzf-open-frecently


alias ocloud="odf $DIR_LIBRARY"



#  ** fasd

# fasd, files -> fzf
#v() {
#  if[ $# -eq 0 ]; then
#    local files="fasd -fl "
#  else#
#
#  fi#
#
#
#  local file
#  file="$(fasd -Rfl "$1" | fzf-tmux -1 -0 --no-sort +m)" && sublime "${file}" || return 1
#}

#  *** z
unalias z 2> /dev/null
# fasd, directories -> fzf
z() {
  # no arguments -> browse in global cache, jump to files or open directory
  # argument:  first, try it for files  (make it restrictive!)
  #            second, pass to  -d to jump to directory
    if [ $# -eq 0 ]; then
    dirs="$(fasd -dl | fzf-tmux -1 -0 --no-sort +m)"
    cd "${dirs}"
  else
    fasd_cd -d $@
  fi
}

#  *** z.
z.() {
  # no arguments -> browse in sub directory, jump to files or open directory
  # arguments    -> try it for files underneath current directory, jump to them
  #                 if that failed -> find directories
  if [ $# -eq 0 ]; then
    dirs="$(find -L . | fzf-tmux -1 -0 --no-sort +m)"
    cd "${dirs}"
  else
    fasd_cd -d $@
  fi
}

#  *** o
# open file using specified commands, add to fasd database
o() {
	case $(xdg-mime query filetype $1) in
		text*|application*script|application*x-yaml)
			$DIR_LOCAL/bin/emacsclient -nw --socket-name=/tmp/emacs1000/server $1 && \
				fasd -A $1 || return 1
			;;
		application*vnd*opendocument*|application*vnd*officedocument*)
			libreoffice $1 || return 1
			;;
		image*)
			geeqie -r -t $1 2>/dev/null || return 1
			;;
		*)
			xdg-open $1 && fasd -A $1 || return 1
	esac
}

gshow(){
  git cat-file blob "$1:$2" > "$1_$2.temp"
  slap "$1_$2.temp"
  rm "$1_$2.temp"
}

#  ** a persistent directory stack
DIRSTACKSIZE=32
DIRSTACKFILE="${HOME}"/.zdirs

autoload -U is-at-least
# Keep dirstack across logouts
if [[ -f ${DIRSTACKFILE} ]] && [[ ${#dirstack[*]} -eq 0 ]] ; then
    dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
    dirstack=( ${(u)dirstack} )
fi

# Make sure there are no duplicates
typeset -U dirstack

# Share dirstack between multiple zsh instances
function chpwd() {
    if is-at-least 4.1; then # dirs -p needs 4.1
        # Get the dirstack from the file and add it to the current dirstack
        dirstack+=( ${(f)"$(< $DIRSTACKFILE)"} )
        dirstack=( ${(u)dirstack} )
        dirs -pl |sort -u >! ${DIRSTACKFILE}
    fi
}


#  ** TODO git:  a selector for selecting modified files to add
#function gadd(){
#
#  # add all
#  addAll=false
#  if[[ $1 = 'a' ]]; then
#    addAll=true
#  fi
#
#  files=$(echo `git diff --name-only` | tr ' ' '\n' | fzf -m --reverse --cycle)
#
#
#
#}

#  ** dict.cc.py
# pip3 install dict.cc.py
function dic(){
    dict.cc.py --max-results 25 en de $1 | colorit | less -r
}

function dic2() {
    {dict -f $1} 2>/dev/null | sed '/No/d' | sed '/localhost/d' | sed "/$1/d" | sed "/From/d" | colorit  | less -r
}


#  ** logging
alias syslog="multitail /var/log/syslog"
alias kernlog="multitail /var/log/kern.log"
#  ** taffybar
alias taffybar="export `dbus-launch`; taffybar"


# ** mail
alias imapfilter="imapfilter -c $DIR_SYSTEM/mail/imapfilter.lua"


#  ** pip
function pip_()
{
	unalias pip
	if [[ "$1" = "install" ]]
	then
		install_path=`echo $PYTHONPATH | sed 's/:.*$//'`
		shift
		pip install --target $install_path $@
	else
		pip $@
	fi
	alias pip=pip_
}
alias pip=pip_

# NOTE: -> alias?
# on exit of shell (but not subshells):   remove traces in /tmp
function zshexit()
{
	if [[ -f /tmp/zsh_keymap_$$ ]]; then
		rm /tmp/zsh_keymap_$$
	fi
}
