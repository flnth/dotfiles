
#█▓▒░ cursor shape on mode change

function zle-line-init zle-keymap-select {
	echo $KEYMAP > /tmp/zsh_keymap_$$
	if [[ -z "$TMUX" ]]; then
		case $KEYMAP in
			viins|main) print -n -- "\e[6 q\e \e]12;#66cd00\007";;
			vicmd)  	print -n -- "\e[2 q\e \e]12;#ffaf00\007";;
		esac
	else
		case $KEYMAP in
			viins|main) print -n -- "\ePtmux;\e\033]12;#66cd00\007\e\033[6 q\e\\";;
			vicmd)  	print -n -- "\ePtmux;\e\033]12;#ffaf00\007\e\033[2 q\e\\";;
			# viins|main) print -n "tmux, viins|main";;
			# vicmd)  	print -n "tmux, vicmd";;
		esac
	fi
}
zle -N zle-keymap-select
zle -N zle-line-init

#█▓▒░ copy / yank / paste

[[ -n $DISPLAY ]] && (( $+commands[xsel] )) && {

	function cutbuffer() {
		zle .$WIDGET
		echo $CUTBUFFER | xsel --clipboard --input
	}

	zle_cut_widgets=(
		vi-backward-delete-char
		vi-change
		vi-change-eol
		vi-change-whole-line
		vi-delete
		vi-delete-char
		vi-kill-eol
		vi-substitute
		vi-yank
		vi-yank-eol
	)
	for widget in $zle_cut_widgets
	do
		zle -N $widget cutbuffer
	done

	function putbuffer() {
		zle copy-region-as-kill "$(xsel -o)"
		zle .$WIDGET
	}

	zle_put_widgets=(
		vi-put-after
		vi-put-before
	)
	for widget in $zle_put_widgets
	do
		zle -N $widget putbuffer
	done
}
