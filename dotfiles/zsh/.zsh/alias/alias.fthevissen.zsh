export PATH="$DIR_ANACONDA/bin:$PATH"

function remove_anaconda_from_path(){
    export PATH=`echo $PATH | sed 's/\/home\/fthevissen\/software\/anaconda\/bin//g'`
}

function meld(){
	# use apt-installed meld and libraries, remove everything from the environment that might get in its way...
	PYTHONPATH="" PATH="/home/fthevissen/perl5/bin:/home/fthevissen/software/pdfsizeopt:/home/fthevissen/.fasd-install:/home/fthevissen/.fasd-install:/home/fthevissen/.npm-packages/bin:/home/fthevissen/.go/bin:/home/fthevissen/software/Qt/5.11.1/gcc_64/bin:/home/fthevissen/.cargo/bin:/home/fthevissen/system:/home/fthevissen/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/snap/bin:/home/fthevissen/.dotnet/tools:/usr/lib/jvm/java-8-oracle/bin:/usr/lib/jvm/java-8-oracle/db/bin:/usr/lib/jvm/java-8-oracle/jre/bin:/home/fthevissen/.fzf/bin" LD_LIBRARY_PATH=""
	/usr/bin/meld "$@"
}

