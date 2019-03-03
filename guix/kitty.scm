(define-module (kitty)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-26))

(define-public kitty
  (package
    (name "kitty")
    (version "0.13.3")
    (home-page "https://sw.kovidgoyal.net/kitty/")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kovidgoyal/kitty.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1y0vd75j8g61jdj8miml79w5ri3pqli5rv9iq6zdrxvzfa4b2rmb"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; patch needed as sphinx-build is used as a python script
           ;; whereas the guix package uses a bash script launching the
           ;; python script
           (substitute* "docs/conf.py"
             (("(from kitty.constants import str_version)" kitty-imp)
              (string-append "sys.path.append(\"..\")\n" kitty-imp)))
           (substitute* "docs/Makefile"
             (("^SPHINXBUILD[[:space:]]+= (python3.*)$")
              "SPHINXBUILD = sphinx-build\n"))
           #t))))
    (build-system gnu-build-system)
    (inputs
     `(("python" ,python)
       ("harfbuzz" ,harfbuzz)
       ("zlib" ,zlib)
       ("libpng" ,libpng)
       ("freetype" ,freetype)
       ("fontconfig" ,fontconfig)
       ("pygments" ,python-pygments)
       ("wayland" ,wayland)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("libxrandr" ,libxrandr)
       ("libdbus" ,dbus)
       ("libxcursor" ,libxcursor)
       ("libxi" ,libxi)
       ("libxinerama" ,libxinerama)
       ("libgl1-mesa" ,mesa)
       ("libxkbcommon" ,libxkbcommon)
       ("sphinx" ,python-sphinx)
       ("ncurses" ,ncurses) ;; for tic command
       ("wayland-protocols" ,wayland-protocols)))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  ;; Wayland backend requires EGL, which isn't found
                  ;; out-of-the-box for some reason. Hard-code it instead.
                  (add-after 'unpack 'hard-code-libegl
                    (lambda _
                      (let* ((mesa (assoc-ref %build-inputs "libgl1-mesa"))
                             (libegl (string-append mesa "/lib/libEGL.so.1")))
                        (substitute* "glfw/egl_context.c"
                                     (("libEGL.so.1") libegl)))
                      #t))
                  (replace 'build
                    (lambda _
                      (invoke "python3" "setup.py" "linux-package")))
                  (replace 'check
                    (lambda _
                      (invoke "python3" "setup.py" "test")))
                  (add-before 'install 'rm-pycache
                    ;; created python cache __pycache__ are non deterministic
                    (lambda _
                      (let ((pycaches (find-files "linux-package/"
                                                  "__pycache__"
                                                  #:directories? #t)))
                        (for-each delete-file-recursively pycaches)
                        #t)))
                  (replace 'install
                    (lambda _
                      (let* ((out (assoc-ref %outputs "out"))
                             (obin (string-append out "/bin"))
                             (olib (string-append out "/lib"))
                             (oshare (string-append out "/share")))
                        (copy-recursively "linux-package/bin" obin)
                        (copy-recursively "linux-package/share" oshare)
                        (copy-recursively "linux-package/lib" olib)
                        #t))))))
    (synopsis "Fast, featureful, GPU based terminal emulator")
    (description "Kitty is a fast and featureful GPU-based terminal emulator:
@itemize
@item Offloads rendering to the GPU for lower system load and buttery smooth
scrolling.  Uses threaded rendering to minimize input latency.
@item Supports all modern terminal features: graphics (images), unicode,
true-color, OpenType ligatures, mouse protocol, focus tracking, bracketed
paste and several new terminal protocol extensions.
@item Supports tiling multiple terminal windows side by side in different
layouts without needing to use an extra program like tmux.
@item Can be controlled from scripts or the shell prompt, even over SSH.
@item Has a framework for Kittens, small terminal programs that can be used to
extend kitty's functionality.  For example, they are used for Unicode input,
hints, and side-by-side diff.
@item Supports startup sessions which allow you to specify the window/tab
layout, working directories and programs to run on startup.
@item Allows you to open the scrollback buffer in a separate window using
arbitrary programs of your choice.  This is useful for browsing the history
comfortably in a pager or editor.
@end itemize")
    (license license:gpl3+)))
