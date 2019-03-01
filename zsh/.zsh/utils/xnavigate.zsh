#!/usr/bin/env zsh

typeset -A tmux_at
tmux_at=(	L "#{pane_at_left}" \
			D "#{pane_at_bottom}" \
			U "#{pane_at_top}" \
			R "#{pane_at_right}")

typeset -A pass_down_keys
pass_down_keys=(	L M-h \
					D M-j \
					U M-k \
					R M-l)

typeset -A xmonadctl_dir
xmonadctl_dir=(	L 1
			  	D 2
			  	U 3
			  	R 4)

in_emacs_or_vim(){
	# if $(tmux display-message -p '#{pane_current_command}' | grep -iqE '(^|\/)n?vim(diff)?$|emacs.*$|^vi$'); then
	if $(tmux display-message -p '#{pane_current_command}' | grep -iqE 'emacs.*$'); then
		true
	else
		false
	fi;
}

at_tmux_border(){
	if [ $(tmux display-message -p "${tmux_at[$1]}" ) -ne 0 ]; then
		true
	else
		false
	fi
}

# [ $(tmux display-message -p "#{pane_at_bottom}") -ne 0 ]; then echo "there"; fi

xnav(){
	# tmux -> "pass keys down to e.g. emacs" 
	# emacs -> xnav.sh -> send keys to tmux or xmonadctl


	# called (first) from either
	#  1) tmux when in tmux (-> tmux.conf) :  passing LRUD
	#  2) zsh when not in tmux             :  passing LRUD
	#  3) term-emacs no matter where       :  passing LRUD false

	# if not in tmux, navigate using xmonadctl
	[[ -z $TMUX ]] && xmonadctl ${xmonadctl_dir[$1]} && return

	# tmux display-message "$1 $2"
	# tmux display-message "I AM IN HERE. WHY?"

	# so we're in tmux...
	pass_down=$2
	# if [ -z "$pass_down" ]; then
	# 	tmux display-message "-z $pass_down"
	# 	pass_down=true
	# fi

	if ($pass_down && in_emacs_or_vim) ; then
		# so we're in emacs or vim
		# tmux display-message "hello?"
		tmux send-keys "${pass_down_keys[$1]}"
	else
		if at_tmux_border $1; then
			xmonadctl ${xmonadctl_dir[$1]}
		else
			tmux select-pane -$1
		fi
	fi
}
