# System-wide environment for florinator
# Intended to be loaded by login-shells, i.e. on system-start

export env_set=1

export PATH="$HOME/.cargo/bin:$PATH"

export STACKROOT="$HOME/localstorage"
export LOC="work" # work
export HOSTNAME="$HOST"

# -- locale
export LC_ALL=en_GB.UTF-8
export LANG=en_GB.UTF-8

# -- mail ----------------------------------------------------------------------
export DIR_MAIL="$STACKROOT/.mail"

# -- sync  ---------------------------------------------------------------------
export DIR_SYSTEM="$STACKROOT/system"
export DIR_LIBRARY="$HOME/cloud"
export DIR_CODEARCHIVE="$STACKROOT/share/code"
export DIR_DOTFILES="$STACKROOT/.cfg"
export DIR_NOTES="$HOME/cloud/82_Notes"
export DIR_ORG="$STACKROOT/share/doc"

# -- gui
export TAFFYBAR_MONITOR=1

export MOUSE_SPEED=-0.8

# -- haskell
export STACK_ROOT="$STACKROOT/.stack"  # stack-internal
export PATH="$STACKROOT/opt/stack:$PATH"

# -- software ------------------------------------------------------------------
export DIR_LOCAL="$STACKROOT/software/local"
export DIR_EMACSD="$STACKROOT/.emacs.d"
export DIR_QT="$STACKROOT/software/Qt/5.11.1"
export BOOST_ROOT="$STACKROOT/libraries/boost_1_66_0"

export DIR_FASD="$STACKROOT/.fasd-install"

export DIR_ANACONDA="$STACKROOT/software/anaconda"
export DIR_QTCREATOR="$STACKROOT/software/qtcreatorp"
export DIR_PYCHARM="$STACKROOT/software/pycharm"
export DIR_SUBLIME="$STACKROOT/software/sublime_text_3/"

# todo: make it so that dirs can be commented out here (explicit checking before
# usage below..!)

# default linux configuration dir:
export XDG_CONFIG_HOME=$HOME/.config

export GUIX_PROFILE="$HOME/.guix-profile"

#export INFOPATH="$DIR_LOCAL/share/info":"$GUIX_PROFILE/share/info"
export PATH="$GUIX_PROFILE/bin:$GUIX_PROFILE/sbin${PATH:+:}$PATH"
export PATH="$HOME/.config/guix/current/bin:$PATH"
#export GUILE_LOAD_PATH="$GUIX_PROFILE/share/guile/site/2.2${GUILE_LOAD_PATH:+:}$GUILE_LOAD_PATH"
#export GUILE_LOAD_COMPILED_PATH="$GUIX_PROFILE/lib/guile/2.2/site-ccache${GUILE_LOAD_COMPILED_PATH:+:}$GUILE_LOAD_COMPILED_PATH"
#export XDG_DATA_DIRS="$GUIX_PROFILE/share${XDG_DATA_DIRS:+:}$XDG_DATA_DIRS"
#export GIO_EXTRA_MODULES="$GUIX_PROFILE/lib/gio/modules${GIO_EXTRA_MODULES:+:}$GIO_EXTRA_MODULES"
export GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"
#export GI_TYPELIB_PATH="$GUIX_PROFILE/lib/girepository-1.0"
export PYTHONPATH="$GUIX_PROFILE/lib/python3.7/site-packages${PYTHONPATH:+:}$PYTHONPATH"
export PYTHONPATH="$GUIX_PROFILE/lib/python2.7/site-packages${PYTHONPATH:+:}$PYTHONPATH"
export TERMINFO_DIRS="$GUIX_PROFILE/share/terminfo${TERMINFO_DIRS:+:}$TERMINFO_DIRS"
#export GUIX_GTK3_PATH="$GUIX_PROFILE/lib/gtk-3.0${GUIX_GTK3_PATH:+:}$GUIX_GTK3_PATH"



export EDITOR='vim'
# #export ALTERNATE_EDITOR=""
# # export TERM=xterm-256color
# # export TERM=screen-256color
export TERM=rxvt-unicode-256color

# # -- paths ----------------------------------------------------------------------
export PKG_CONFIG_PATH="$DIR_LOCAL/lib/pkgconfig"
export GOPATH="$STACKROOT/.go"
export PATH="$GOPATH/bin:$PATH"

export PATH="/sbin:/usr/sbin:$PATH"
[[ -e "$HOME/bin" ]] && export PATH="$HOME/bin:$PATH"

#export PATH="$DIR_ANACONDA/bin:$PATH"
#export PATH="$DIR_QT/gcc_64/bin:$PATH"
#export PATH="$HOME/software/local/bin:$PATH"
#export PATH="$NPM_PACKAGES/bin:$PATH"
#export PATH="$HOME/.local/bin:$PATH"
export PATH="$DIR_LOCAL/lib/python/bin:$PATH"
export PATH="$STACKROOT/.dotfiles/bin:$PATH"  # TODO:  hardcoding of STACKROOT/.dotfiles

# use distcc
export PATH="/usr/lib/distcc:$PATH"

export MANPATH="$STACKROOT/share/man:$MANPATH"
export MANWIDTH=100

# configuration
export ZDOTDIR="$HOME" # TODO: change?

# mlocate databases
export MLOCATEPNAMES="build .emacs.d"
export MLOCATEDIRS="$HOME/system:$HOME/ApodiusCode"
export MLOCATEDBS="$HOME/.updatedb/system.db:$HOME/.updatedb/apodius.db"

#export PATH="$HOME/software/taskjuggler/bin"

# custom site-packages directory for python
export PYTHONPATH="$DIR_LOCAL/lib/python:$PYTHONPATH"

# make libraries in /lib folder in $DIR_LOCAL useable (NOTE: not propagating to shells...)
# export LD_LIBRARY_PATH="$DIR_LOCAL/lib:$LD_LIBRARY_PATH"

# -- node/npm
NPM_PACKAGES="${HOME}/.npm-packages"
# Unset manpath so we can inherit from /etc/manpath via the `manpath` command
#unset MANPATH # delete if you already modified MANPATH elsewhere in your config export MANPATH="$NPM_PACKAGES/share/man:$(manpath)"
#export PATH="$NPM_PACKAGES/bin:$PATH"

# -- perl
PATH="$HOME/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="$HOME/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="$HOME/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"$HOME/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"; export PERL_MM_OPT;

# -- urxvt
# TODO:  if this exists...
export PATH="$STACKROOT/opt/urxvt/bin":$PATH
export PERL5LIB="$STACKROOT/opt/urxvt/perl_inc":$PERL5LIB

# -- gpgagent
export GPGAGENT=/usr/bin/gpg-agent

export DOTNET_CLI_TELEMETRY_OPTOUT=1

echo "sourced $0"
