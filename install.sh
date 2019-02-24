#!/usr/bin/zsh
setopt sh_word_split
echo_(){ echo "\n\033[0;31m█▓▒░ \033[0;32m$1\033[0m\n" }
table(){ printf "%-10s %-30s\n" $1 $2 }

#█▓▒░ environment

echo_ "Checking environment..."
if [[ -z $STACKROOT ]]; then
	read -q "response?...not set, configure? (y/n)"
	echo "\n"
	if [[ $response == (y|yes|Y) ]]; then
		target=env.$(hostname).zsh
		cp system/env.template.zsh $target
		sed -i -e "s/<TEMPLATE>/$(hostname)/g" $target
		vim $target
		source $target
	else
		exit
	fi
fi

table "  STACKROOT: " $STACKROOT
table "  ZDOTDIR: "   $ZDOTDIR

echo "\n"
read -q "response?   continue? (y/n) "
if [[ ! $response == (y|yes|Y) ]]; then
	exit
fi


echo "\n"
#█▓▒░ system packages

echo_ "Installing system packages..."
. install/install-system-packages.sh system/packages


echo "\n"
#█▓▒░ guix

read -q "response?install guix packages? (y/n) "
echo "\n"
if [[ $response == (y|yes|Y) ]]; then
	echo_ "Installing guix..."
	$(install/install-guix.sh)
	# TODO:  application setup (locales, etc.
	echo_ "Installing official guix packages..."
	. install/install-guix-packages.sh guix/official
	echo_ "Installing custom guix packages..."
	script_dir=$(dirname "$(readlink -f "$0")")
	export GUIX_PACKAGE_PATH=$script_dir/guix/
	. install/install-guix-packages.sh guix/custom
fi

echo "\n"
#█▓▒░ other binaries

echo_ "Installing other binaries..."

# fzf
echo "installing fzf..."
go get -u github.com/junegunn/fzf


#█▓▒░ dotfiles

# gitconfig
if [[ ! -f "git/.gitconfig" ]]; then
   echo_ "Creating git/gitconfig.$(hostname) and git/.gitconfig ..."
   . install/create-gitconfig.sh git/gitconfig.template git/gitconfig.$(hostname)
   ln -s gitconfig.$(hostname) git/.gitconfig
   git add git/gitconfig.$(hostname)
fi

# symlinks
dirs="x tmux git"
echo_ "Symlinking dotfiles..."

echo "...zsh -> $ZDOTDIR"
# stow zsh -t $ZDOTDIR  # TODO        uncomment

echo "...($dirs)"
for dir in $dirs; do
	stow $dir -t /tmp
done
