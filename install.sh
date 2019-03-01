#!/usr/bin/zsh
setopt sh_word_split
echo_(){ echo "\n\033[0;31m█▓▒░ \033[0;32m$1\033[0m\n" }
table(){ printf "%-10s %-30s\n" $1 $2 }

#█▓▒░ environment

echo_ "Checking environment..."
env_file=.env.$(hostname).sh
if [[ -z $STACKROOT ]] || [[ ! -e "$HOME/$env_file" ]]; then
	read -q "response?...not set or existant, configure? (y/n)"
	echo "\n"
	if [[ $response == (y|yes|Y) ]]; then
		cp system/env.template.sh system/$env_file
		sed -i -e "s/<TEMPLATE>/$(hostname)/g" system/$env_file
		vim system/$env_file
		git add system/$env_file
	else
		exit
	fi
	source system/$env_file
	echo "gopath:  $GOPATH"
fi

table "  STACKROOT: " $STACKROOT
table "  ZDOTDIR: "   $ZDOTDIR
table "  GOPATH: "   $GOPATH

echo "\n"
read -q "response?   continue? (y/n) "
if [[ ! $response == (y|yes|Y) ]]; then
	exit
fi


echo "\n"
#█▓▒░ system packages

echo_ "Installing system packages..."
. install/install-system-packages.sh system/packages

# realtime-/lowlatency kernel
[[ $(uname -a) == *"Ubuntu"* ]] && sudo apt install linux-lowlatency
[[ $(uname -a) == *"Debian"* ]] && sudo apt install linux-image-rt-amd64

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
[[ ! -f $GOPATH/bin/fzf ]] && go get -u github.com/junegunn/fzf

# xmonadctl
echo "building xmonadctl..."
cd x && ghc --make xmonadctl.hs && rm {xmonadctl.o,xmonadctl.hi} && \
mv xmonadctl ../bin
cd ..

# ycmd
echo "installing ycmd..."
rev="6d8ddd5d6b5b9c2f885cfd5e589231d30d3c7360"

[[ ! -d "$STACKROOT/opt" ]] && mkdir "$STACKROOT/opt"
if [[ ! -d "$STACKROOT/opt/ycmd" ]]; then
	cd "$STACKROOT/opt"
	git clone https://github.com/Valloric/ycmd.git
	cd ycmd
	git checkout $rev
	git submodule update --init --recursive
	python3 build.py --clang-completer
fi
# TODO: ycmd directories in emacs, maybe even put into path


echo "\n"
#█▓▒░ dotfiles

# gitconfig
if [[ ! -f "git/.gitconfig" ]]; then
   echo_ "Creating git/gitconfig.$(hostname) and git/.gitconfig ..."
   . install/create-gitconfig.sh git/gitconfig.template git/gitconfig.$(hostname)
   ln -s gitconfig.$(hostname) git/.gitconfig
   git add git/gitconfig.$(hostname)
fi

# symlinks
dirs="x tmux git fonts"
echo_ "Symlinking dotfiles..."

echo "...zsh into $ZDOTDIR"
# stow zsh -t $ZDOTDIR  # TODO        uncomment

echo "...($dirs)"
for dir in $dirs; do
	stow $dir -t $HOME
done
