#!/usr/bin/zsh
# # --------------------------------------------------------
# Does two things:
#   - build git-credential-libsecret if necessary
#   - create gitconfig from template (expecting <MAIL> entry)
#
# Expect two arguments:
#   - path to template file
#   - path to target file
# # --------------------------------------------------------

usage(){ echo "usage:   create-gitconfig.sh template target" }
[[ "$#" -ne 2 ]] && usage && exit 1


cur_dir=`pwd`
template=$1
target=$2

#█▓▒░ git-credential-libsecret

apps_list="libsecret-1-0  libsecret-1-dev"
sudo apt install -y $(echo $apps_list)

if [[ ! -f $HOME/.git/git-credential-libsecret &&  -f "/usr/share/doc/git/contrib/credential/libsecret/Makefile" ]]; then
	[[ ! -d "$HOME/.git" ]] && mkdir ~/.git
	cp /usr/share/doc/git/contrib/credential/libsecret/* ~/.git
	cd ~/.git && make && echo
	if [[ -f "$HOME/.git/git-credential-libsecret" ]]; then
		echo "  git-credential-libsecret successfully built."
	else
		echo "  error building git-credential-libsecret."
	fi
	cd $cur_dir
fi

#█▓▒░ create $(host)_gitconfig

[[ -L $target || -f $target ]] && rm $target

# substitute mail address
echo "\nSubstituting mail address in $target..."
echo "mail-address> "
read mailaddy
cp $template $target
sed -i -e "s/<MAIL>/$mailaddy/g" $target

# build credential helper under ~/.git
if [[ -f "$HOME/.git/git-credential-libsecret" ]]; then
	echo "  git-credential-libsecret found"
	echo "\n" >> $target
	echo "[credential]" >> $target
	echo "\thelper = $HOME/.git/git-credential-libsecret" >> $target
else
	echo "  No git-credential-libsecret found, file $target will be missing this entry."
fi

echo "..$target created from $template .\n"



