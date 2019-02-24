
#█▓▒░ zsh-specific environment

if [ -n "$TMUX" ]; then
	export TERM=rxvt-unicode-256color # tmux term causes problems
else
	export TERM=rxvt-unicode-256color
fi

# this belongs into .env files, but if set there is not propagated :/
# export LD_LIBRARY_PATH="$DIR_LOCAL/lib:$LD_LIBRARY_PATH"
# export PKG_CONFIG_PATH="$DIR_LOCAL/lib/pkgconfig"

# gpg-agent
if [ -f "${STACKROOT}/.gpg-agent-info" ]; then
	. "${STACKROOT}/.gpg-agent-info"
	export GPG_AGENT_INFO
	export SSH_AUTH_SOCK
fi

