(define-module (emacs-from-source)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)		; for librsvg
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages linux)		; alsa-lib
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages pcre)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public emacs-from-source
  (let ((commit "af047384594c73ed87947d5ecbbfc1032435b769")
		(hash "07c4w457j90z0sspbjwfwgrjkn42jsl8xvarvk8z8hadcybpy2cd"))
	(package
	 (name "emacs-from-source")
	 (version "27.0.50")
	 (source (origin
			  (method git-fetch)
			  (uri (git-reference
					(url "https://github.com/emacs-mirror/emacs.git")
					(commit commit)))
			  (sha256 (base32 hash))
			  (modules '((guix build utils)))
			  (snippet
			   ;; Delete the bundled byte-compiled elisp files and
			   ;; generated autoloads.
			   '(with-directory-excursion
				 "lisp"
				 (for-each delete-file
						   (append (find-files "." "\\.elc$")
								   (find-files "." "loaddefs\\.el$")))

				 ;; Make sure Tramp looks for binaries in the right places on
				 ;; remote GuixSD machines, where 'getconf PATH' returns
				 ;; something bogus.
				 (substitute* "net/tramp-sh.el"
							  ;; Patch the line after "(defcustom tramp-remote-path".
							  (("\\(tramp-default-remote-path")
							   (format #f "(tramp-default-remote-path ~s ~s ~s ~s "
									   "~/.guix-profile/bin" "~/.guix-profile/sbin"
									   "/run/current-system/profile/bin"
									   "/run/current-system/profile/sbin")))

				 ;; Make sure Man looks for C header files in the right
				 ;; places.
				 (substitute* "man.el"
							  (("\"/usr/local/include\"" line)
							   (string-join
								(list line
									  "\"~/.guix-profile/include\""
									  "\"/var/guix/profiles/system/profile/include\"")
								" ")))
				 #t))))
	 (build-system gnu-build-system)
	 (arguments '(#:configure-flags '("--with-x-toolkit=lucid")
									#:tests? #f
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
												   )
									))
	 (inputs
	  `(("gnutls" ,gnutls)
		("ncurses" ,ncurses)

		;; TODO: Add the optional dependencies.
		("libx11" ,libx11)
		("libxaw" ,libxaw)
		("gtk+" ,gtk+)
		("libxft" ,libxft)
		("libtiff" ,libtiff)
		("giflib" ,giflib)
		("libjpeg" ,libjpeg)
		("imagemagick" ,imagemagick)
		("acl" ,acl)

		;; When looking for libpng `configure' links with `-lpng -lz', so we
		;; must also provide zlib as an input.
		("libpng" ,libpng)
		("zlib" ,zlib)

		("librsvg" ,librsvg)
		("libxpm" ,libxpm)
		("libxml2" ,libxml2)
		("libice" ,libice)
		("libsm" ,libsm)
		("alsa-lib" ,alsa-lib)
		("dbus" ,dbus)

		;; multilingualization support
		("libotf" ,libotf)
		("m17n-lib" ,m17n-lib)))
	 (native-inputs
	  `(
		;; ("guix-emacs.el" ,(search-auxiliary-file "emacs/guix-emacs.el"))
		("autoconf" ,autoconf)
		("automake" ,automake)
		("pkg-config" ,pkg-config)
		("texinfo" ,texinfo)))
	 (propagated-inputs
	  `(("pcre" ,pcre)
		("pcre2" ,pcre2))
	  )
	 (native-search-paths
	  (list (search-path-specification
			 (variable "INFOPATH")
			 (files '("share/info")))))

	 (home-page "https://www.gnu.org/software/emacs/")
	 (synopsis "The extensible, customizable, self-documenting text editor")
	 (description
	  "GNU Emacs is an extensible and highly customizable text editor.  It is
based on an Emacs Lisp interpreter with extensions for text editing.  Emacs
has been extended in essentially all areas of computing, giving rise to a
vast array of packages supporting, e.g., email, IRC and XMPP messaging,
spreadsheets, remote server editing, and much more.  Emacs includes extensive
documentation on all aspects of the system, from basic editing to writing
large Lisp programs.  It has full Unicode support for nearly all human
languages.")
	 (license license:gpl3+))))

(define-public m17n-db
  (package
    (name "m17n-db")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/m17n/m17n-db-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0vfw7z9i2s9np6nmx1d4dlsywm044rkaqarn7akffmb6bf1j6zv5"))))
    (build-system gnu-build-system)
    (inputs
     `(("gettext" ,gettext-minimal)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-charmaps="
                            (assoc-ref %build-inputs "libc")
                            "/share/i18n/charmaps"))))
    ;; With `guix lint' the home-page URI returns a small page saying
    ;; that your browser does not handle frames. This triggers the "URI
    ;; returns suspiciously small file" warning.
    (home-page "https://www.nongnu.org/m17n/")
    (synopsis "Multilingual text processing library (database)")
    (description "The m17n library realizes multilingualization of
many aspects of applications.  The m17n library represents
multilingual text as an object named M-text.  M-text is a string with
attributes called text properties, and designed to substitute for
string in C.  Text properties carry any information required to input,
display and edit the text.

This package contains the library database.")
    (license license:lgpl2.1+)))

(define-public m17n-lib
  (package
    (name "m17n-lib")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/m17n/m17n-lib-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0jp61y09xqj10mclpip48qlfhniw8gwy8b28cbzxy8hq8pkwmfkq"))))
    (build-system gnu-build-system)
    (inputs
     `(("fribidi" ,fribidi)
       ("gd" ,gd)
       ("libotf" ,libotf)
       ("libxft" ,libxft)
       ("libxml2" ,libxml2)
       ("m17n-db" ,m17n-db)))
    (arguments
     `(#:parallel-build? #f))
    ;; With `guix lint' the home-page URI returns a small page saying
    ;; that your browser does not handle frames. This triggers the "URI
    ;; returns suspiciously small file" warning.
    (home-page "https://www.nongnu.org/m17n/")
    (synopsis "Multilingual text processing library (runtime)")
    (description "The m17n library realizes multilingualization of
many aspects of applications.  The m17n library represents
multilingual text as an object named M-text.  M-text is a string with
attributes called text properties, and designed to substitute for
string in C.  Text properties carry any information required to input,
display and edit the text.

This package contains the library runtime.")
    (license license:lgpl2.1+)))

