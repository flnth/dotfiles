
export KEYTIMEOUT=1

# vi keybindings
bindkey -v

#  * vicmd
_physical_up_line()   { zle backward-char -n $COLUMNS }
_physical_down_line() { zle forward-char  -n $COLUMNS }
zle -N physical-up-line _physical_up_line
zle -N physical-down-line _physical_down_line
bindkey -M vicmd "j" physical-down-line
bindkey -M vicmd "k" physical-up-line

bindkey -M viins "^?" backward-delete-char

backward-kill-selective () {
    # local WORDCHARS=${WORDCHARS/\/}
    local WORDCHARS=
    zle backward-kill-word
}
zle -N backward-kill-selective
bindkey '^[^?' backward-kill-selective
# bindkey '^[^?' vi-backward-kill-word

#  * navigation

cdUndoKey() {
  popd      > /dev/null
  zle       reset-prompt
}

cdParentKey() {
  cd ..
  zle      reset-prompt
}

zle -N                 cdParentKey
zle -N                 cdUndoKey
bindkey "^[[1;5A"      cdParentKey
bindkey "^[[1;5D"      cdUndoKey
bindkey -M vicmd "^[[1;5A"      cdParentKey
bindkey -M vicmd "^[[1;5D"      cdUndoKey

# bindkey "^[^[[A"      cdParentKey
# bindkey "^[^[[D"      cdUndoKey
# bindkey -M vicmd "^[^[[A"      cdParentKey
# bindkey -M vicmd "^[^[[D"      cdUndoKey

# TODO: implement a "redo" function for these keys.
#       for that: maintain my own stack whenever one (or both?) of them are used
#       clear stack when necessary, maybe instrument cd for that


#  * history

# tab/C-tab
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
# C-n/C-p
bindkey '^P' history-substring-search-up
bindkey '^N' history-substring-search-down



#  * menuselect
# when to enter menuselect?

#  ** enter
#-- immediately
# setopt menu_complete

#-- manually via C-tab
menu-select(){
	# TODO need to clean up something here? e.g. leave list selection / restore
	# original commandline state?
	zle menu-select
}

bindkey '^[[3;1A' menu-select


#-- repeated TAB
# setopt auto_menu      # after repeatedly pressing TAB

#  ** leave
bindkey -M menuselect '\C-g' send-break
bindkey -M menuselect '\033' send-break


#  ** keys

#-- some commands --------------------------------------------------------------------
# accept-line:   accept current match and leave menu selection (do not accept command)
#                NOTE:  add a dot before ALL these functions to execute fn and accept line
# send-break:    leave menu selection and restore previous contents of command line
# accept-and-hold: accept current match and continue selection NOTE: on C-o already
# accept-and-infer-next-history:  accept match and then retry completion! NOTE: to tab?

#-- vi navigation
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

#-- selection via TAB and C-n/C-p
bindkey -M menuselect '^[[Z' reverse-menu-complete
bindkey -M menuselect '^[[3;1A' reverse-menu-complete
bindkey -M menuselect '^p' vi-up-line-or-history      # TODO: why vi functions here?
bindkey -M menuselect '^n' vi-down-line-or-history

bindkey -M menuselect 'u' undo
#-- forward-backward page
# bindkey -M menuselect '\C-d' vi-backward-word     # back full page, \w wraparound
# bindkey -M menuselect '\C-u' vi-forward-word-end  # forward full page NOTE:  better to just cancel it all? clear line?

#  ** fzf

bindkey '^[f' fzf-Mf
bindkey -M vicmd '^[f' fzf-Mf

fzf-history-widget() {
	local selected num
	setopt localoptions noglobsubst noposixbuiltins pipefail 2> /dev/null
	selected=( $(fc -rl 1 |
					 FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS --query=${(qqq)LBUFFER} +m" $(__fzfcmd)) )
	local ret=$?
	if [ -n "$selected" ]; then
		num=$selected[1]
		if [ -n "$num" ]; then
			zle vi-fetch-history -n $num
		fi
	fi
	zle reset-prompt
	return $ret
}
zle     -N   fzf-history-widget
bindkey '^R' fzf-history-widget
bindkey -M vicmd '^R' fzf-history-widget


#  ** tmux
function tmux_copy_mode_down(){
	tmux copy-mode
	tmux send-keys -X cursor-down
}
zle -N        tmux_copy_mode_down
bindkey -M vicmd '^j' tmux_copy_mode_down

function tmux_copy_mode_up(){
	tmux copy-mode
	tmux send-keys -X cursor-up
}
zle -N        tmux_copy_mode_up
bindkey -M vicmd '^k' tmux_copy_mode_up

# function tmux_copy_mode_up_page(){
# 	tmux copy-mode
# 	tmux send-keys -X halfpage-up
# }
# zle -N       tmux_copy_mode_up_page
# bindkey -M vicmd '^U' tmux_copy_mode_up_page
bindkey -M vicmd '^U' vi-kill-line

function tmux_copy_mode_search_forward(){
	tmux copy-mode;
	tmux send-keys "/"
}
zle -N        tmux_copy_mode_search_forward
bindkey -M vicmd "^[/" tmux_copy_mode_search_forward
bindkey -M vicmd "\/" tmux_copy_mode_search_forward
bindkey -M vicmd "//" tmux_copy_mode_search_forward
bindkey -M vicmd "/" tmux_copy_mode_search_forward

function tmux_copy_mode_search_backward(){
	tmux copy-mode;
	tmux send-keys "?"
}
zle -N        tmux_copy_mode_search_backward
bindkey -M vicmd '?' tmux_copy_mode_search_backward

# function tmux_select_current_line(){
# 	tmux copy-mode;
# 	tmux send-keys "C-v"
# }
# zle -N       tmux_select_current_line()
# bindkey -M viins

# -- tmux
# function zle-keymap-select zle-line-init
# {
#     # change cursor shape in iTerm2
#     case $KEYMAP in
#         vicmd)      print -n -- "\ePtmux;\e\033]12;#66cd00\007\e\033[6 q\e\\";;  # block cursor
#         viins|main) print -n -- "\ePtmux;\e\033]12;#ffaf00\007\e\033[2 q\e\\";;  # line cursor
#     esac

#     zle reset-prompt
#     zle -R
# }

# function zle-line-finish
# {
#     print -n -- "\E]50;CursorShape=0\C-G"  # block cursor
# }

# zle -N zle-line-init
# zle -N zle-line-finish
# zle -N zle-keymap-select


# ------ equivalent spacemacs-like functionality in vicmd mode (!)
bindkey -M vicmd -r ' '

bindkey -M vicmd ' gs' magit-status
bindkey -M vicmd ' gl' magit-log
bindkey -M vicmd ' pf' fzf-pf
bindkey -M vicmd ' ff' fzf-ff
bindkey -M vicmd 'gb' fzf-gb
bindkey -M vicmd ' gC' fzf-gC
bindkey -M vicmd ' u gC' fzf-ugC

bindkey -M vicmd ' zd' fasd-zd
bindkey -M vicmd ' zf' fasd-zf
# bindkey -M vicmd ' zp' fasd-zp
bindkey -M vicmd ' ad' fzf-ad

tmux-scroll-up() { tmux copy-mode; tmux send-keys -X -N 5 scroll-up }; zle -N tmux-scroll-up

bindkey -M viins '^u' backward-kill-line
bindkey -M vicmd '^u' tmux-scroll-up

# bindkey -M vicmd ' ps' ag or something like it?
# bindkey -M vicmd 'gb' frecent list --> emacs?

xnavL() { xnav L  }; zle -N xnavL
xnavD() { xnav D  }; zle -N xnavD
xnavU() { xnav U  }; zle -N xnavU
xnavR() { xnav R  }; zle -N xnavR

if [ -z $TMUX ]; then
	bindkey -M vicmd '^[h' xnavL
	bindkey -M vicmd '^[j' xnavD
	bindkey -M vicmd '^[k' xnavU
	bindkey -M vicmd '^[l' xnavR

	bindkey -M viins '^[h' xnavL
	bindkey -M viins '^[j' xnavD
	bindkey -M viins '^[k' xnavU
	bindkey -M viins '^[l' xnavR
fi
