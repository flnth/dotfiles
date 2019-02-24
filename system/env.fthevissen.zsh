# System-wide environment for <template>
# Intended to be loaded by login-shells, i.e. on system-start

export env_set=1

export PATH="$HOME/.cargo/bin:$PATH"

export STACKROOT='$HOME'
export LOC="home" # work
export HOSTNAME=$HOST

# -- mail ----------------------------------------------------------------------
export DIR_MAIL="$HOME/.mail"

# -- sync  ---------------------------------------------------------------------
export DIR_SYSTEM="$STACKROOT/system"
export DIR_LIBRARY="/home/fthevissen/cloud"
export DIR_CODEARCHIVE="/home/fthevissen/cloud/41    Code"
export DIR_DOTFILES="$STACKROOT/.cfg"
export DIR_NOTES="/home/fthevissen/cloud/82_Notes"
export DIR_ORG="/home/fthevissen/cloud/82_Notes/org"

# -- gui
export DIR_XMONAD="/home/fthevissen/system/xmonad"
export TAFFYBAR_MONITOR=1

export MOUSE_SPEED=-0.8

# -- haskell
export STACK_ROOT="/home/fthevissen/.stack"

# -- software ------------------------------------------------------------------
export DIR_LOCAL="$STACKROOT/software/local"
export DIR_EMACSD="$DIR_SYSTEM/.emacs.d/"
export DIR_QT="$STACKROOT/software/Qt/5.11.1"
export BOOST_ROOT="$STACKROOT/libraries/boost_1_66_0"

export DIR_FZF="$STACKROOT/.fzf"
export DIR_FASD="$STACKROOT/.fasd-install"

export DIR_ANACONDA="$STACKROOT/software/anaconda"
export DIR_QTCREATOR="$STACKROOT/software/qtcreatorp"
export DIR_PYCHARM="$STACKROOT/software/pycharm"
export DIR_SUBLIME="$STACKROOT/software/sublime_text_3/"

# todo: make it so that dirs can be commented out here (explicit checking before
# usage below..!)

export INFOPATH="$DIR_LOCAL/share/info":"$HOME/.guix-profile/share/info"

export PATH="/home/fthevissen/.guix-profile/bin:/home/fthevissen/.guix-profile/sbin${PATH:+:}$PATH"
export GUILE_LOAD_PATH="/home/fthevissen/.guix-profile/share/guile/site/2.2${GUILE_LOAD_PATH:+:}$GUILE_LOAD_PATH"
export GUILE_LOAD_COMPILED_PATH="/home/fthevissen/.guix-profile/lib/guile/2.2/site-ccache${GUILE_LOAD_COMPILED_PATH:+:}$GUILE_LOAD_COMPILED_PATH"
export XDG_DATA_DIRS="/home/fthevissen/.guix-profile/share${XDG_DATA_DIRS:+:}$XDG_DATA_DIRS"
export GIO_EXTRA_MODULES="/home/fthevissen/.guix-profile/lib/gio/modules${GIO_EXTRA_MODULES:+:}$GIO_EXTRA_MODULES"
export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"


export EDITOR='emacsclient -t -a=\"\"'
#export ALTERNATE_EDITOR=""
# export TERM=xterm-256color
# export TERM=screen-256color
export TERM=rxvt-unicode-256color

# -- paths ----------------------------------------------------------------------
export PKG_CONFIG_PATH="$DIR_LOCAL/lib/pkgconfig"
export GOPATH="/home/fthevissen/.go"

export PATH="$DIR_ANACONDA/bin:$PATH"
export PATH="$DIR_QT/gcc_64/bin:$PATH"
export PATH="$HOME/software/local/bin:$PATH"
export PATH="$NPM_PACKAGES/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"
export PATH="$DIR_LOCAL/lib/python/bin:$PATH"
if [[ ! "$PATH" == *${DIR_FZF}/bin* ]]; then
	export PATH="$PATH:${DIR_FZF}/bin"
fi

export MANPATH="$DIR_LOCAL/man:$DIR_LOCAL/share/man:$DIR_SYSTEM/doc/man:$MANPATH"
export MANWIDTH=100

# configuration
export ZDOTDIR="$DIR_SYSTEM/terminal/zdotdir"  # NOTE: new:  set to ~

# mlocate databases
export MLOCATEPNAMES="build .emacs.d"
export MLOCATEDIRS="/home/fthevissen/system:/home/fthevissen/ApodiusCode"
export MLOCATEDBS="/home/fthevissen/.updatedb/system.db:/home/fthevissen/.updatedb/apodius.db"

#export PATH="$HOME/software/taskjuggler/bin"

# custom site-packages directory for python
export PYTHONPATH="$DIR_LOCAL/lib/python:$PYTHONPATH"

# make libraries in /lib folder in $DIR_LOCAL useable (NOTE: not propagating to shells...)
export LD_LIBRARY_PATH="$DIR_LOCAL/lib:$LD_LIBRARY_PATH"

# -- node/npm
NPM_PACKAGES="${HOME}/.npm-packages"
# Unset manpath so we can inherit from /etc/manpath via the `manpath` command
#unset MANPATH # delete if you already modified MANPATH elsewhere in your config export MANPATH="$NPM_PACKAGES/share/man:$(manpath)"
export PATH="$NPM_PACKAGES/bin:$PATH"

# -- gpgagent
export GPGAGENT=/usr/bin/gpg-agent

export DOTNET_CLI_TELEMETRY_OPTOUT=1

echo "sourced env.fthevissen.sh"
