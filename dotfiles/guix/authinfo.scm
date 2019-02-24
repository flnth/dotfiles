(define-module (authinfo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (guix utils)
  #:use-module (gnu packages gnupg)
  )

(define-public authinfo
  (let ((commit "89cbf3a8f64ce79e317b37677636831cad274e03")
		(hash "0avpphicfkbx8kakqdlkx51wqafm5mbvj21lnxa00d5camd167zk"))
	(package
	 (name "authinfo")
	 (version "0.4")
	 (source (origin
			  (method git-fetch)
			  (uri (git-reference
					(url "https://github.com/aartamonau/authinfo.git")
					(commit commit)))
			  (sha256 (base32 hash))
			  (modules '((guix build utils)))))
	 (build-system gnu-build-system)
	 ;; (arguments
	 ;;  `(#:phases
	 ;; 	(modify-phases %standard-phases
	 ;; 				   (add-after 'install 'install-authinfo-python
	 ;; 							  (lambda* (#:key outputs #:allow-other-keys)
	 ;; 									   (copy-file "python/authinfo.py"
	 ;; 												  (string-append (assoc-ref outputs "out")
	 ;; 												  				 "/lib/python2.7/authinfo/authinfo.py"
	 ;; 												  				 )
	 ;; 												  )
	 ;; 									   #t)))
	 ;; 	)
	 ;;  )
	 (inputs
	  `(
		("python" ,python-2)
		("libtool" ,libtool)
		("gnupg" ,gnupg)
		("gpgme" ,gpgme)
		("libassuan" ,libassuan)
	 	))
	 (native-inputs
	  `(
		("pkg-config" ,pkg-config)
	 	("autoconf" ,autoconf)
	 	("automake" ,automake)
	 	))
	 ;; (native-search-paths
	 ;;  (list (search-path-specification
	 ;; 		 (variable "INFOPATH")
	 ;; 		 (files '("share/info")))))

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
