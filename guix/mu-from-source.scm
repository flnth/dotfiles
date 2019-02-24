
  (define-module (mu-from-source)
  #:use-module (gnu packages)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  ;; #:use-module (gnu packages dbm)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages django)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gsasl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  ;; #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  ;; #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages search)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages screen)
  ;; #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses)
                #:select (fdl1.1+
                           agpl3+
                           gpl2 gpl2+ gpl3 gpl3+ lgpl2.1 lgpl2.1+ lgpl3+
                           non-copyleft (expat . license:expat) bsd-3
                           public-domain bsd-4 isc (openssl . license:openssl)
                           bsd-2 x11-style agpl3 asl2.0 perl-license))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial))

(define-public gmime
  (package
    (name "gmime")
    (version "3.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gmime/"
                                  (version-major+minor version)
                                  "/gmime-" version ".tar.xz"))
              (sha256
               (base32
                "1q6palbpf6lh6bvy9ly26q5apl5k0z0r4mvl6zzqh90rz4rn1v3m"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gnupg" ,gnupg)))               ; for tests only
    (inputs `(("glib" ,glib)
              ("gpgme" ,gpgme)
              ("zlib" ,zlib)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'patch-paths-in-tests
          (lambda _
            ;; The test programs run several programs using 'system' with
            ;; hard-coded paths.  Here we patch them all.
            ;; We use ISO-8859-1 here because test-iconv.c contains
            ;; raw byte sequences in several different encodings.
            (with-fluids ((%default-port-encoding #f))
              (substitute* (find-files "tests" "\\.c$")
                (("(system *\\(\")(/[^ ]*)" all pre prog-path)
                 (let* ((base (basename prog-path))
                        (prog (which base)))
                   (string-append pre
                                  (or prog (error "not found: " base)))))))
            #t)))))
    (home-page "http://spruce.sourceforge.net/gmime/")
    (synopsis "MIME message parser and creator library")
    (description
     "GMime provides a core library and set of utilities which may be used for
the creation and parsing of messages using the Multipurpose Internet Mail
Extension (MIME).")
    (license (list lgpl2.1+ gpl2+ gpl3+))))

;; Some packages are not ready for GMime 3 yet.
(define-public gmime-2.6
  (package
    (inherit gmime)
    (version "2.6.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gmime/"
                                  (version-major+minor version)
                                  "/gmime-" version ".tar.xz"))
              (sha256
               (base32
                "0slzlzcr3h8jikpz5a5amqd0csqh2m40gdk910ws2hnaf5m6hjbi"))))))

(define-public mu-from-source
  (let ((commit "9cf120b012cbe7ee4ef52299c1c0d66d08d32800" )
		(hash "1jz0sqfq2vz27yq3jjx3fmv9ygxis67pcac435a6a51cxi98i6ba"))
	(package
     (name "mu-from-source")
     (version "1.1")
     (source (origin
              (method git-fetch)
              (uri (git-reference
					(url "https://github.com/djcb/mu.git")
					(commit commit)))
              (sha256 (base32 hash))))
     (build-system gnu-build-system)
     (native-inputs
      `(
		("makeinfo" ,texinfo)
		("libtool" ,libtool)
		("autoconf" ,autoconf)
		("automake" ,automake)
		("pkg-config" ,pkg-config)
		("glib" ,glib "bin")			; for gtester
		("emacs" ,emacs-minimal)
		("tzdata" ,tzdata-for-tests)))	;for mu/test/test-mu-query.c
     ;; TODO: Add webkit and gtk to build the mug GUI.
     (inputs
      `(("xapian" ,xapian)
		("guile" ,guile-2.2)
		("glib" ,glib)
		("gmime" ,gmime)))
     (arguments
      `(#:modules ((guix build gnu-build-system)
                   (guix build utils)
                   (guix build emacs-utils))
				  #:imported-modules (,@%gnu-build-system-modules
									  (guix build emacs-utils))
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
								 (add-after 'unpack 'patch-configure
											;; By default, elisp code goes to "share/emacs/site-lisp/mu4e",
											;; so our Emacs package can't find it.  Setting "--with-lispdir"
											;; configure flag doesn't help because "mu4e" will be added to
											;; the lispdir anyway, so we have to modify "configure.ac".
											(lambda _
											  (zero? (system* "sh" "./autogen.sh"))
											  (substitute* "configure"
														   (("^ +lispdir=\"\\$\\{lispdir\\}/mu4e/\".*") "")
														   ;; Use latest Guile
														   (("guile-2.0") "guile-2.2"))
											  (substitute* '("guile/Makefile.in"
															 "guile/mu/Makefile.in")
														   (("share/guile/site/2.0/") "share/guile/site/2.2/"))
											  #t))
								 (add-after 'patch-configure 'fix-date-tests
											;; Loosen test tolerances to prevent failures caused by daylight
											;; saving time (DST).  See: https://github.com/djcb/mu/issues/1214.
											(lambda _
											  (substitute* "lib/parser/test-utils.cc"
														   (("\\* 60 \\* 60, 1 },")
															"* 60 * 60, 3600 + 1 },"))
											  #t))
								 (add-before 'install 'fix-ffi
											 (lambda* (#:key outputs #:allow-other-keys)
													  (substitute* "guile/mu.scm"
																   (("\"libguile-mu\"")
																	(format #f "\"~a/lib/libguile-mu\""
																			(assoc-ref outputs "out"))))
													  #t))
								 (add-before 'check 'check-tz-setup
											 (lambda* (#:key inputs #:allow-other-keys)
													  ;; For mu/test/test-mu-query.c
													  (setenv "TZDIR"
															  (string-append (assoc-ref inputs "tzdata")
																			 "/share/zoneinfo"))
													  #t))
								 (add-after 'install 'install-emacs-autoloads
											(lambda* (#:key outputs #:allow-other-keys)
													 (emacs-generate-autoloads
													  "mu4e"
													  (string-append (assoc-ref outputs "out")
																	 "/share/emacs/site-lisp"))
													 #t)))))
     (home-page "http://www.djcbsoftware.nl/code/mu/")
     (synopsis "Quickly find emails")
     (description
      "Mu is a tool for dealing with e-mail messages stored in the
Maildir-format.  Mu's purpose in life is to help you to quickly find the
messages you need; in addition, it allows you to view messages, extract
attachments, create new maildirs, and so on.")
     (license gpl3+))))
