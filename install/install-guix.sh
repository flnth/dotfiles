#!/usr/bin/env sh

# Usage: install-guix.sh

# Installation script for Guix
# Only run this script if Guix has not yet been installed on the system
# This script will likely need to run with root privileges

if [ $(which guix) ]; then
	# echo "Already installed"
	exit
fi

binaryFile='guix-binary-0.16.0.x86_64-linux.tar.xz';

downloadBinary() {
	echo "downloading binary ..."
	sigFile="$binaryFile.sig"
	if [ ! -e $binaryFile ]; then
		wget "ftp://alpha.gnu.org/gnu/guix/$binaryFile"
	fi
	wget "ftp://alpha.gnu.org/gnu/guix/$sigFile" -O $sigFile
	gpg --keyserver pgp.mit.edu --recv-keys 3CE464558A84FDC69DB40CFB090B11993D9AEBB5
	gpg --verify $sigFile
}

unpackTar() {
	echo "unpacking ..."
	if [ ! -e /gnu ]; then
		tar --warning=no-timestamp -xf $binaryFile \
			&& mv var/guix /var/ \
			&& mv gnu /
	fi

}

initializeRootProfile() {
	echo "initializing root profile ..."
	# Make root’s profile available
	mkdir -p ~root/.config/guix
	if [ -e ~root/.config/guix/current ]; then rm -rf ~root/.config/guix/current; fi
	ln -sf /var/guix/profiles/per-user/root/current-guix \
		~root/.config/guix/current
	# Source etc/profile to augment PATH and other relevant environment variables
	GUIX_PROFILE="`echo ~root`/.config/guix/current"
	echo $GUIX_PROFILE
	echo "$GUIX_PROFILE/etc/profile"
	sudo -s source $GUIX_PROFILE/etc/profile
}

# Create the group and user accounts for build users
createGroupAndUserAccountsForBuildUsers() {
	echo "Creating group and user account for build users ..."
	if ! grep -i '^guixbuild:' /etc/group; then
		groupadd --system guixbuild
	fi
	for i in `seq -w 1 4`; do
		if ! id "guixbuilder$i" >/dev/null 2>&1; then
			useradd -g guixbuild -G guixbuild \
					-d /var/empty -s `which nologin` \
					-c "Guix build user $i" --system  \
					guixbuilder$i;
		fi
	done

}

setupDaemon() {
	echo "Setting up daemon ..."
	# Run the daemon, and set it to automatically start on boot
	# If host distro uses the systemd init system
	if [ $(which systemctl) ]; then
		cp ~root/.config/guix/current/lib/systemd/system/guix-daemon.service \
		   /etc/systemd/system/
		systemctl start guix-daemon && systemctl enable guix-daemon
		# If host distro uses the Upstart init system
	elif [ $(which initctl) ]; then
		initctl reload-configuration
		cp ~root/.config/guix/current/lib/upstart/system/guix-daemon.conf /etc/init/
		if ! echo "$(status guix-daemon)" | grep 'running'; then
			start guix-daemon
		fi
	fi

}

# Make the guix command available to other users on the machine
makeGuixCommandAvailable() {
	echo "Provide guix command to all users on the machine ..."
	mkdir -p /usr/local/bin
	cd /usr/local/bin
	if [ -L /usr/local/bin/guix ]; then rm /usr/local/bin/guix; fi
	ln -s /var/guix/profiles/per-user/root/current-guix/bin/guix

}

# Make the Info version of the manual available
makeInfoAvailable() {
	echo "Providing info version of guix manual ..."
	mkdir -p /usr/local/share/info
	cd /usr/local/share/info
	for i in /var/guix/profiles/per-user/root/current-guix/share/info/* ; do
		if [ ! -L "$i" ]; then
			echo $i
			ln -s $i
		fi
	done

}

# Authorize substitutes from hydra.gnu.org
authorizeSubstitutes() {
	echo "Authorizing substitutes..."
	guix archive --authorize < ~root/.config/guix/current/share/guix/ci.guix.info.pub

}

setupGuix() {
	cd /tmp \
	    && downloadBinary \
		&& unpackTar \
		&& initializeRootProfile \
		&& createGroupAndUserAccountsForBuildUsers \
		&& setupDaemon \
		&& makeGuixCommandAvailable \
		&& makeInfoAvailable \
		&& authorizeSubstitutes

}

setupGuix
echo "... done!"
