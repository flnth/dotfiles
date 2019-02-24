# expects fasd executable to be in $PATH

# cache
export FASD_DATA="$STACKROOT/.fasd"
fasd_cache="$ZDOTDIR/.zsh/fasd-init.sh"
if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
    fasd --init posix-alias zsh-hook zsh-ccomp-install zsh-ccomp zsh-wcomp zsh-wcomp-install >| "$fasd_cache"
fi
source "$fasd_cache"
unset fasd_cache
