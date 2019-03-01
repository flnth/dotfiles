# ~/.profile: executed for login shells
#
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.
# see /usr/share/doc/bash/examples/startup-files for examples. the files are
# located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
# umask 022

if [ -n "$BASH_VERSION" ]; then
	# if running bash, include .bashrc
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

if [ -d "$HOME/bin" ] ; then
	# include private bin, if existant
    PATH="$HOME/bin:$PATH"
    PATH="$HOME/system:$PATH"
fi

env="$HOME/.env.$(uname -n).sh"
if [ -z $env_set ] && [ -e $env ]; then
	# TODO: lightweight configuration for when STACKROOT is not available, i.e.
	# on a foreign localstorage
	. $env
fi

# keyboard setup
if [ -x `which loadkeys` ] && [ $(find `which loadkeys` -perm -0001) ]; then
	# load uk key layout, put escape on capslock
	loadkeys uk
	loadkeys <<EOF
	keymaps 0-15
	keycode 58 = Escape
	keycode 1 = Escape
EOF
else
	  echo "Couldn't set keyboard layout - setuid bit not set on /bin/loadkeys ?"
fi

# gpg-agent
#[ ! -z $GPGAGENT ] && \
#	$GPGAGENT --daemon --enable-ssh-support \
#			  --write-env-file "$STACKROOT/.gpg-agent-info" >& /dev/null

# java fix
export _JAVA_AWT_WM_NONREPARENTING=1

# emacs daemon
[ ! -z $DIR_SYSTEM ] && [ -f "$DIR_SYSTEM/emacsd" ] && "$DIR_SYSTEM/emacsd"

# owncloud
if [ "$LOC" = "home" ] && [ $(command -v owncloud) ] ; then
    owncloud &
fi
