(define-module (urxvt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages bison)
  #:use-module (ice-9 match))


(define-public urxvt
  (let ((commit "6f2540ee849d4d6e7e1fd03e3f3544becd102c7f")
		(hash "0ymsmqj77mr08x0g2k84ywarwy9zs105mm3z72fqc493z6wcaqz4"))
	(package
	 (name "urxvt")
	 (version "9.22d")
	 (source (origin
			  (method git-fetch)
			  (uri (git-reference
					(url "https://github.com/flnth/rxvt-unicode")
					(commit commit)))
			  (sha256 (base32 hash))))
	 (build-system gnu-build-system)
	 (arguments
	  `(#:configure-flags '("--enable-frills"
							"--enable-256-color"
							"--enable-xft"
							"--enable-wide-glyphs"
							"--enable-iso14755")
						  ;; This sets the destination when installing the necessary terminal
						  ;; capability data, which are not provided by 'ncurses'.  See
						  ;; https://lists.gnu.org/archive/html/bug-ncurses/2009-10/msg00031.html
		#:make-flags (list (string-append "TERMINFO="
										  (assoc-ref %outputs "out")
										  "/share/terminfo"))
		#:phases
		(modify-phases %standard-phases
					   (add-after 'unpack 'make-git-checkout-writable
								  (lambda _
									(for-each make-file-writable (find-files "."))
									#t))
					   (add-before 'configure 'fix-/bin/pwd
								   (lambda _
									 ;; Use `pwd', not `/bin/pwd'.
									 (substitute* (find-files "." "^Makefile\\.in$")
												  (("/bin/pwd")
												   "pwd"))
									 #t))
					   (add-after 'install 'install-desktop-urxvt
								  (lambda* (#:key outputs #:allow-other-keys)
										   (let* ((output (assoc-ref outputs "out"))
												  (desktop (string-append output "/share/applications")))
											 (mkdir-p desktop)
											 (with-output-to-file
												 (string-append desktop "/urxvt.desktop")
											   (lambda _
												 (format #t
														 "[Desktop Entry]~@
						   Name=rxvt-unicode~@
						   Comment=~@
						   Exec=~a/bin/urxvt~@
						   TryExec=~@*~a/bin/urxvt~@
						   Icon=~@
						   Type=Application~%"
														 output)))
											 #t)))
					   (add-after 'install 'install-desktop-urxvtc
								  (lambda* (#:key outputs #:allow-other-keys)
										   (let* ((output (assoc-ref outputs "out"))
												  (desktop (string-append output "/share/applications")))
											 (mkdir-p desktop)
											 (with-output-to-file
												 (string-append desktop "/urxvtc.desktop")
											   (lambda _
												 (format #t
														 "[Desktop Entry]~@
						   Name=rxvt-unicode (client)~@
						   Comment=Rxvt clone with XFT and unicode support~@
						   Exec=~a/bin/urxvtc~@
						   TryExec=~@*~a/bin/urxvtc~@
						   Icon=~@
						   Type=Application~%"
														 output)))
											 #t))))))
	 (inputs
	  `(("libXft" ,libxft)
		("libX11" ,libx11)))
	 (native-inputs
	  `(("ncurses" ,ncurses)		  ;trigger the installation of terminfo data
		("perl" ,perl)
		("pkg-config" ,pkg-config)))
	 ;; FIXME: This should only be located in 'ncurses'.  Nonetheless it is
	 ;; provided for usability reasons.  See <https://bugs.gnu.org/22138>.
	 (native-search-paths
	  (list (search-path-specification
			 (variable "TERMINFO_DIRS")
			 (files '("share/terminfo")))))
	 (home-page "http://software.schmorp.de/pkg/rxvt-unicode.html")
	 (synopsis "Rxvt clone with XFT and unicode support")
	 (description "Rxvt-unicode (urxvt) is a colour vt102 terminal emulator
intended as an xterm replacement for users who do not require features such as
Tektronix 4014 emulation and toolkit-style configurability.  It supports
unicode, XFT and may be extended with Perl plugins.  It also comes with a
client/daemon pair that lets you open any number of terminal windows from
within a single process.")
	 (license license:gpl3+))))
