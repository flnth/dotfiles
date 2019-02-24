setopt extendedglob

autoload colors && colors
HISTFILE=$STACKROOT/.zsh_history

# autoloading/completion
# fpath=($ZDOTDIR/completion $fpath)

#█▓▒░ Package Management:   Antigen

this_dir=$(dirname "$(readlink -f "$0")")
export ANTIGEN_HS_HOME=$this_dir/antigen-hs   # src
export ANTIGEN_HS_MY=$ANTIGEN_HS_HOME/my-antigen.hs
export ANTIGEN_HS_OUT=$STACKROOT/.antigen-hs  # repos/packages
source $ANTIGEN_HS_HOME/init.zsh

#█▓▒░ pushd
setopt pushd_ignore_dups   # no duplicates on dir stack
setopt pushd_silent        # no dir stack after pushd or popd
setopt pushd_to_home       # `pushd` = `pushd $HOME`
setopt auto_pushd          # auto pushd

#█▓▒░ logcheck
watch=all
logcheck=30
WATCHFMT="%n from %M has %a tty%l at %T %W"

