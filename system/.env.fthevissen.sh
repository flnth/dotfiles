
export env_set=1

export PATH="$HOME/.cargo/bin:$PATH"
#export TESTENV="testenv set"

export STACKROOT='/home/fthevissen'
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
export DIR_EMACSD="/home/fthevissen/.emacs.d/"
export DIR_QT="$STACKROOT/software/Qt/5.11.1"
export BOOST_ROOT="$STACKROOT/libraries/boost_1_66_0"

export DIR_FZF="$STACKROOT/.fzf"
export DIR_FASD="$STACKROOT/.fasd-install"
export PATH=$DIR_FASD:$PATH # TODO:  do erase
export FASD_DATA="$STACKROOT/.fasd"

export DIR_ANACONDA="$STACKROOT/software/anaconda"
export DIR_QTCREATOR="$STACKROOT/software/qtcreatorp"
export DIR_PYCHARM="$STACKROOT/software/pycharm"
export DIR_SUBLIME="$STACKROOT/software/sublime_text_3/"

# todo: make it so that dirs can be commented out here (explicit checking before
# usage below..!)

export GUIX_PROFILE="$STACKROOT/.guix-profile"

export INFOPATH="$DIR_LOCAL/share/info":"$GUIX_PROFILE/share/info"
export PATH="$GUIX_PROFILE/bin:$GUIX_PROFILE/sbin${PATH:+:}$PATH"
export PATH="$HOME/.config/guix/current/bin:$PATH"
export PATH="$HOME/.guix-profile/bin:$PATH"
export GUILE_LOAD_PATH="$GUIX_PROFILE/share/guile/site/2.2${GUILE_LOAD_PATH:+:}$GUILE_LOAD_PATH"
export GUILE_LOAD_COMPILED_PATH="$GUIX_PROFILE/lib/guile/2.2/site-ccache${GUILE_LOAD_COMPILED_PATH:+:}$GUILE_LOAD_COMPILED_PATH"
export XDG_DATA_DIRS="$GUIX_PROFILE/share${XDG_DATA_DIRS:+:}$XDG_DATA_DIRS"
export GIO_EXTRA_MODULES="$GUIX_PROFILE/lib/gio/modules${GIO_EXTRA_MODULES:+:}$GIO_EXTRA_MODULES"
export GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"
export GI_TYPELIB_PATH="$GUIX_PROFILE/lib/girepository-1.0"
export PYTHONPATH="$GUIX_PROFILE/lib/python3.7/site-packages${PYTHONPATH:+:}$PYTHONPATH"
export PYTHONPATH="$GUIX_PROFILE/lib/python2.7/site-packages${PYTHONPATH:+:}$PYTHONPATH"
# export GIT_EXEC_PATH="$GUIX_PROFILE/libexec/git-core"
export TERMINFO_DIRS="$GUIX_PROFILE/share/terminfo${TERMINFO_DIRS:+:}$TERMINFO_DIRS"
export GUIX_GTK3_PATH="$GUIX_PROFILE/lib/gtk-3.0${GUIX_GTK3_PATH:+:}$GUIX_GTK3_PATH"

export CPATH="/home/fthevissen/.guix-profile/include${CPATH:+:}$CPATH"
export LIBRARY_PATH="/home/fthevissen/.guix-profile/lib${LIBRARY_PATH:+:}$LIBRARY_PATH"
export QMAKEPATH="/home/fthevissen/.guix-profile/lib/qt5${QMAKEPATH:+:}$QMAKEPATH"
export QML2_IMPORT_PATH="/home/fthevissen/.guix-profile/lib/qt5/qml${QML2_IMPORT_PATH:+:}$QML2_IMPORT_PATH"
export XDG_CONFIG_DIRS="/home/fthevissen/.guix-profile/etc/xdg${XDG_CONFIG_DIRS:+:}$XDG_CONFIG_DIRS"
export C_INCLUDE_PATH="/home/fthevissen/.guix-profile/include${C_INCLUDE_PATH:+:}$C_INCLUDE_PATH"
export CPLUS_INCLUDE_PATH="/home/fthevissen/.guix-profile/include${CPLUS_INCLUDE_PATH:+:}$CPLUS_INCLUDE_PATH"
export CMAKE_PREFIX_PATH="/home/fthevissen/.guix-profile/${CMAKE_PREFIX_PATH:+:}$CMAKE_PREFIX_PATH"


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
export ZDOTDIR="/home/fthevissen"  # NOTE: new:  set to ~

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
