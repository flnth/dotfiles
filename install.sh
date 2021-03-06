#!/usr/bin/zsh
setopt sh_word_split
echo_(){ echo "\n\033[0;31m█▓▒░ \033[0;32m$1\033[0m\n" }
table(){ printf "%-10s %-30s\n" $1 $2 }

# ----------------------------------------------------------
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
# ----------------------------------------------------------
#█▓▒░ system packages

echo_ "Installing system packages..."
. install/install-system-packages.sh system/packages

# realtime-/lowlatency kernel
[[ $(uname -a) == *"Ubuntu"* ]] && sudo apt install linux-lowlatency
[[ $(uname -a) == *"Debian"* ]] && sudo apt install linux-image-rt-amd64

# symlinks


echo "\n"
# ----------------------------------------------------------
#█▓▒░ guix

read -q "response?install guix packages? (y/n) "
echo "\n"
if [[ $response == (y|yes|Y) ]]; then
	echo_ "Installing guix..."
	$(install/install-guix.sh)
	echo "Updating guix..."
	export PATH="$HOME/.config/guix/current/bin/guix"
	guix pull
	guix package -u
	echo_ "Installing official guix packages..."
	. install/install-guix-packages.sh guix/official
	echo_ "Installing custom guix packages..."
	script_dir=$(dirname "$(readlink -f "$0")")
	export GUIX_PACKAGE_PATH=$script_dir/guix/
	. install/install-guix-packages.sh guix/custom
fi

echo "\n"
# ----------------------------------------------------------
#█▓▒░ other binaries

echo_ "Installing other binaries..."

[[ ! -d "$STACKROOT/opt" ]] && mkdir "$STACKROOT/opt"

# link $STACKROOT/opt -> $HOME/opt
if [[ ! -e $HOME/opt ]]; then
	[[ -L $HOME/opt ]] && rm $HOME/opt  # invalid symlink
	$( cd $HOME && ln -s $STACKROOT/opt opt)
fi

# urxvt
echo "installing urxvt..."
if [[ ! $(command -v urxvt) ]] ; then
	cd $STACKROOT/opt
	git clone https://github.com/flnth/rxvt-unicode.git urxvt
	cd urxvt
	rm -rf bin
	mkdir bin
	./configure --enable-frills --enable-256-color --enable-unicode3 --enable-xft --enable-font-styles --enable-wide-glyphs --enable-iso14755
	make -j4 && \
		cd bin && \
		ln -s ../src/rxvt urxvt && \
		ln -s ../src/rxvtc urxvtc && \
		ln -s ../src/rxvtd urxvtd

fi

# fzf
echo "installing fzf..."
[[ ! -f $GOPATH/bin/fzf ]] && go get -u github.com/junegunn/fzf

# ripgrep, fd
echo "installing fd,ripgrep..."
cargo install fd
cargo install ripgrep

# stack, xmonad, taffybar
echo "installing and updating stack..."
cd /tmp
curl -sSL https://get.haskellstack.org/ > stack_install.sh
chmod +x stack_install.sh
./stack_install.sh -d $STACKROOT/opt/stack

echo "building xmonad..."
cd $STACKROOT/.dotfiles/x/.xmonad  # TODO:  hardcoded path into this repo...
stack build
./build xmonad-x86_64-linux  # TODO: need to hardcode...?

# xmonadctl
echo "building xmonadctl..."
cd x && ghc --make xmonadctl.hs && rm {xmonadctl.o,xmonadctl.hi} && \
	mv xmonadctl ../bin
cd ..

# ccls
if [[ ! -d "$STACKROOT/opt/ccls" ]]; then
	mkdir -p "$STACKROOT/opt/ccls"
	cd "$STACKROOT/opt/ccls"
	git clone https://github.com/MaskRay/ccls.git .
	git checkout 400a77c
	wget https://releases.llvm.org/8.0.0/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-16.04.tar.xz
	tar -xvf clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-16.04.tar.xz
	mv "clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-16.04" clang
	cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=$PWD/clang
	cd Release && make -j4
fi

# visual studio code for gdb/lldb debugging
if [[ ! $(command -v code) ]]; then
	wget -q https://packages.microsoft.com/keys/microsoft.asc -O- | sudo apt-key add -
	sudo apt update && sudo apt install code
fi

# yed
if [[ $(command -v yed) ]]; then
	filename="yEd-3.19_with-JRE11_64-bit_setup.sh"
	wget -q https://www.yworks.com/resources/yed/demo/$filename
	chmod +x $filename
	./$filename
fi

# fman
if [[ $(command -v fman) ]]; then
	sudo apt-key adv --keyserver keyserver.ubuntu.com --recv 9CFAF7EB
	sudo apt-get install apt-transport-https
	echo "deb [arch=amd64] https://fman.io/updates/ubuntu/ stable main" | sudo tee /etc/apt/sources.list.d/fman.list
	sudo apt-get update
	sudo apt-get install fman
fi

echo "\n"
# ----------------------------------------------------------
#█▓▒░ directory structure
echo_ "Creating directories, cloning repositories..."

mkdir $STACKROOT/share
cd $STACKROOT/share
[[ ! -d code ]] && git clone https://etft@bitbucket.org/efth/codearchive.git code
[[ ! -d doc ]] && git clone https://efth@bitbucket.org/efth/org.git doc

echo "\n"
# ----------------------------------------------------------
#█▓▒░ dotfiles

# gitconfig
if [[ ! -e "$HOME/.gitconfig" ]]; then
   echo_ "Creating git/gitconfig.$(hostname) and git/.gitconfig ..."
   [[ -L "$HOME/.gitconfig" ]] && rm $HOME/.gitconfig
   . install/create-gitconfig.sh git/gitconfig.template git/gitconfig.$(hostname)
   ln -s gitconfig.$(hostname) git/.gitconfig
   git add git/gitconfig.$(hostname)
fi

# symlinks
echo_ "Symlinking dotfiles..."
dirs="x tmux git fonts highlight share system tmux xdg zsh git etc"

#echo "...zsh into $ZDOTDIR"
# stow zsh -t $ZDOTDIR  # TODO: ZDOTDIR != $HOME, maybe?

cd $STACKROOT/.dotfiles
[[ -e ~/.authinfo.gpg ]] && rm ~/.authinfo.gpg
[[ -f etc/etc/priv/.authinfo.gpg ]] && ln -s $STACKROOT/.dotfiles/etc/etc/priv/.authinfo.gpg ~/.authinfo.gpg

echo "...($dirs)"
for dir in $dirs; do
	# TODO:  what if some files already exist...? check beforehand? output error?
	stow $dir -t $HOME
done

echo "\n"
# ----------------------------------------------------------
#█▓▒░ emacs
echo_ "Emacs configuration..."
cd $STACKROOT
rm -rf .emacs.d
git clone https://github.com/flnth/emacs.d.git .emacs.d
cd .emacs.d
git submodule update --init --recursive
cd $HOME && ln -s $STACKROOT/.emacs.d/.spacemacs .spacemacs








