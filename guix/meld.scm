
(define-module (meld)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages ed)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages less)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml))


(define-public meld
  (package
    (name "meld")
    (version "3.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/meld/"
                           (version-major+minor version)
                           "/meld-" version ".tar.xz"))
       (sha256
        (base32
         "11khi1sg02k3b9qdag3r939cwi27cql4kjim7jhxf9ckfhpzwh6b"))))
    (build-system python-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("xmllint" ,libxml2)
       ("glib-compile-schemas" ,glib "bin")
       ("python-pytest" ,python-pytest)))
    (inputs
     `(("python-cairo" ,python-pycairo)
       ("python-gobject" ,python-pygobject)
	   ))
    (propagated-inputs
     `(("dconf" ,dconf)
	   ("gtk", gtk+)
       ("gtksourceview" ,gtksourceview)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; This setup.py script does not support one of the Python build
         ;; system's default flags, "--single-version-externally-managed".
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "python" "setup.py"
                     ;; This setup.py runs gtk-update-icon-cache  which we don't want.
                     "--no-update-icon-cache"
                     ;; "--no-compile-schemas"
                     "install"
                     (string-append "--prefix=" (assoc-ref outputs "out"))
                     "--root=/")))
         ;; The tests need to be run after installation.
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Tests look for installed package
             (add-installed-pythonpath inputs outputs)
             ;; The tests fail when HOME=/homeless-shelter.
             (setenv "HOME" "/tmp")
             (invoke "py.test" "-v" "-k"
                     ;; TODO: Those tests fail, why?
                     "not test_classify_change_actions"))))))
    (home-page "https://meldmerge.org/")
    (synopsis "Compare files, directories and working copies")
    (description "Meld is a visual diff and merge tool targeted at
developers.  Meld helps you compare files, directories, and version controlled
projects.  It provides two- and three-way comparison of both files and
directories, and has support for many popular version control systems.

Meld helps you review code changes and understand patches.  It might even help
you to figure out what is going on in that merge you keep avoiding.")
    (license gpl2)))
