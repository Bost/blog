(define-module (guix scripts find)
  ;; #:use-module (system syntax) ;; for syntax?
  #:use-module (system syntax internal) ;; for make-syntax syntax?
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module ((guix scripts build) #:select (%standard-build-options))
  #:use-module ((guix diagnostics)
                #:select (location-file location-line))
  #:use-module (gnu packages)
  ;; first take remove delete-duplicates append-map etc.
  #:use-module (srfi srfi-1) ;; remove
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-26)
  #:use-module (guix build po)  ; read-po-file - for translation
  #:use-module (guix build utils) ;; find-files
  #:export (%editor
            show
            guix-findm))

(define (partial fun . args)
  (lambda x (apply fun (append args x))))

(define-public (read-all reader-function)
  "Returns a function which reads all lines of text from the PORT and applies
READER-FUNCTION on them. "
  (lambda (port)
    (let loop ((res '())
               (str (reader-function port))) ; from (ice-9 popen)
      (if (and str (not (eof-object? str)))
          (loop (append res (list str))
                (reader-function port))
          res))))

(define %options
  (list (find (lambda (option)
                (member "load-path" (option-names option)))
              %standard-build-options)
        (option '(#\h "help") #f #f
                (lambda args
                  (leave-on-EPIPE (show-help))
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix findm")))))

(define (show-help)
  (display (G_ "Usage: guix findm MESSAGE...
TODO <guix findm description>
\n"))
  (newline)
  (display (G_ "
  -L, --load-path=DIR    prepend DIR to the package module search path"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

;; from /home/bost/dev/guix/gnu/packages.scm
(define %distro-root-directory
  ;; Absolute file name of the module hierarchy.  Since (gnu packages …) might
  ;; live in a directory different from (guix), try to get the best match.
  (letrec-syntax ((dirname* (syntax-rules ()
                              ((_ file)
                               (dirname file))
                              ((_ file head tail ...)
                               (dirname (dirname* file tail ...)))))
                  (try      (syntax-rules ()
                              ((_ (file things ...) rest ...)
                               (match (search-path %load-path file)
                                 (#f
                                  (try rest ...))
                                 (absolute
                                  (dirname* absolute things ...))))
                              ((_)
                               #f))))
    (try ("gnu/packages/base.scm" gnu/ packages/)
         ("gnu/packages.scm"      gnu/)
         ("guix.scm"))))

(define (search-file file-name)
  "Search the FILE-NAME non-recursively.  Return #f if not found.
E.g.
(search-file \"test.txt\")
"
  (map (cut find-files <> file-name)
       (list %distro-root-directory)))


(define-public (syntax->list orig-ls)
  "From $der/racket/pkgs/racket-benchmarks/tests/racket/benchmarks/common/psyntax-input.txt

(syntax->list (call-with-input-string \"  (+ 1 2)\" read-syntax))
"
  (let f ((ls orig-ls))
    (syntax-case ls ()
      (() '())
      ((x . r) (cons (syntax x) (f (syntax r))))
      (_ (error 'syntax->list "invalid argument ~s" orig-ls)))))

(define (first-match haystack-list-stx needle-datum)
  (cond
   ((null? haystack-list-stx) #f)
   (else
    (or (contains-sexp-syntax? (car haystack-list-stx) needle-datum)
        (first-match (cdr haystack-list-stx) needle-datum)))))

(define (contains-sexp-syntax? haystack-syntax needle-datum)
  "TODO also return the top-level haystack-syntax

(define module-stx (call-with-input-string \"(list aaa \\\"foo\\\")\" read-syntax))
(contains-sexp-syntax? module-stx \"foo\")   ; => #<syntax:unknown file:1:10 \"foo\">
(contains-sexp-syntax? module-stx 'aaa)    ; => #<syntax:unknown file:1:6 aaa>
"
  (let ((haystack-datum (syntax->datum haystack-syntax)))
    (cond
     ((equal? haystack-datum needle-datum) haystack-syntax)
     ((not (list? haystack-datum)) #f)
     (else
      (or (first-match (syntax->list haystack-syntax) needle-datum)
          #f)))))

;; TODO test which read-all-sexps implementation is faster
(define (read-all-sexps file-name)
  (call-with-input-file file-name (read-all read-syntax)))

(define (read-all-sexps file-name)
  (reverse
   (call-with-input-file file-name
     (lambda (input-port)
       (let loop ((module-stx (read-syntax input-port))
                  (result '()))
         (cond
          [(eof-object? module-stx) result]
          [else (loop (read-syntax input-port)
                      (cons module-stx result))]))))))

(define (read-all-sexps file-name)
  (call-with-input-file file-name
    (lambda (input-port)
      (let loop ((module-stx (read-syntax input-port))
                 (result '()))
        (cond
         [(eof-object? module-stx) result] ;; (or (not module-stx) (eof-object? module-stx))
         [else (loop (read-syntax input-port)
                     (append result (list module-stx)))])))))

(define (all-module-syntaxes)
  "Syntaxes of all s-expressions in all scm-files. I.e. syntaxes of modules."
  (map read-all-sexps (scm-files)))

(define (scm-files)
  ;; (find-files (getenv "dgx") "\\.scm$")
  (list
   "/home/bost/dev/guix/guix/platforms/mips.scm"
   "/home/bost/dev/guix/gnu/installer/newt/hostname.scm"
   "/home/bost/dev/guix/guix/platforms/or1k.scm"
   "/home/bost/dev/guix/guix/platforms/avr.scm"
   "/home/bost/dev/guix/build-aux/convert-xref.scm"
   "/home/bost/dev/guix/gnu/installer/hostname.scm"
   "/home/bost/dev/guix/doc/he-config-bare-bones.scm"
   "/home/bost/dev/guix/doc/package-hello.scm"
   "/home/bost/dev/guix/doc/environment-gdb.scm"
   ))

(define (po-files)
  (find-files (getenv "dgx") "\\.po$")
  #;
  (list
   ;; "/home/bost/dev/guix/po/doc/guix-cookbook.de.po"
   ;; "/home/bost/dev/guix/po/doc/guix-cookbook.fr.po"
   ;; "/home/bost/dev/guix/po/doc/guix-cookbook.ko.po"
   ;; "/home/bost/dev/guix/po/doc/guix-cookbook.sk.po"
   ;; "/home/bost/dev/guix/po/doc/guix-manual.de.po"
   ;; "/home/bost/dev/guix/po/doc/guix-manual.es.po"
   ;; "/home/bost/dev/guix/po/doc/guix-manual.fr.po"
   ;; "/home/bost/dev/guix/po/doc/guix-manual.pt_BR.po"
   ;; "/home/bost/dev/guix/po/doc/guix-manual.ru.po"
   ;; "/home/bost/dev/guix/po/doc/guix-manual.zh_CN.po"

   ;; "/home/bost/dev/guix/po/guix/bn.po"
   ))

(define (true? x) (eq? x #t))
(define (false? x) (eq? x #f))

(define (show locations)
  "Show the LOCATIONS, a list of <location> records, and exit."
  (catch 'system-error
    (lambda ()
      (format #t "[show] locations:\n~a\n\n" locations)
      (format #t "[show] %load-path:\n~a\n\n" %load-path)
      (format #t "[show] %distro-root-directory:\n~a\n\n" %distro-root-directory)

      ;;/home/bost/dev/guix/guix/scripts/findm.scm
      #;
      (let [(file-name
             ;; "manifest.scm"
             "\\.scm$"
             #;"findm.scm")]
        (format #t "[show] (search-file ~s):\n~a\n\n" file-name (search-file file-name)))

      (let [(file-name "/home/bost/dev/guix/gnu/installer/newt/hostname.scm")]
        ((compose
          #;(partial format #t "[show] (read-all-sexps ~s):\n~a\n\n"
                   file-name)
          read-all-sexps)
         file-name))

      (exit 0))
    (lambda args
      (let ((errno (system-error-errno args)))
        (leave (G_ "~a~%") (strerror errno))))))


(define-command (guix-findm . args)
  (category development)
  (synopsis "view message definitions")

  (define (parse-arguments)
    ;; Return the list of package names.
    (parse-command-line args %options (list (list))
                #:build-options? #f
                #:argument-handler cons))

  (with-error-handling
    (let* ((message     (parse-arguments))
           (locations (map identity message)))
      (when (null? locations)
        (leave (G_ "no message-locations found~%")))
      (show locations))))

(define (read-all-pofile file-name)
  "Returns a list of string-string pairs:
((\"<english-text>\" . \"<other-lang-text>\") ...)

Can't cope with e.g.:
#: gnu/system/file-systems.scm:137
#, scheme-format
msgid \"invalid file system mount flag:~{ ~s~}~%\"
msgid_plural \"invalid file system mount flags:~{ ~s~}~%\"
msgstr[0] \"option de montage non valide : ~{ ~s~}~%\"
msgstr[1] \"options de montage non valides : ~{ ~s~}~%\"

Could it be that the read-po-file is simply not used that much?
Ie there are some other mechanism how the po-file are read?

E.g.
(read-all-pofile \"/home/bost/dev/guix/po/guix/fr.po\")
;; => ((\"module ~a not found\" . \"module ~a introuvable\")) ...)
"
  (call-with-input-file file-name read-po-file))

(define (find-in-cell-tail needle cell)
  "
(find-in-cell-tail \"d\" (cons \"c\" (list \"d\" \"e\" \"f\"))) ; => \"c\"
(find-in-cell-tail \"e\" (cons \"c\" (list \"d\" \"e\" \"f\"))) ; => \"c\"
(find-in-cell-tail \"f\" (cons \"c\" (list \"d\" \"e\" \"f\"))) ; => \"c\"
(find-in-cell-tail \"f\" (cons \"c\" \"f\" #;(list \"d\" \"e\" \"f\"))) ; => \"c\"
(find-in-cell-tail \"e\" (cons \"c\" (list \"d\" \"e\" \"f\"))) ; => \"c\"
(find-in-cell-tail \"d\" (cons \"c\" (list \"d\" \"e\" \"f\"))) ; => \"c\"
(find-in-cell-tail \"d\" (cons \"c\" (list \"d\" \"e\" #;\"f\"))) ; => \"c\"
(find-in-cell-tail \"e\" (cons \"c\" (list \"d\" \"e\" #;\"f\"))) ; => \"c\"

(find-in-cell-tail \"_\" (cons \"c\" \"f\")) ; => #f
"
  (let* [(cell-tail (cdr cell))]
    (cond
     [(string? cell-tail)
      (when (string=? needle cell-tail)
        (car cell))]
     [(list? cell-tail)
      (when (member needle cell-tail)
        (car cell))])))

(define (test-parse po-files)
  "Test if the po-files can be parsed.
E.g.
(test-parse (po-files))
"
  (map (lambda (f)
         ((compose
           ;; (partial list f)
           (partial format #t "~a\n")
           length+
           read-all-pofile
           (lambda (f)
             (format #t "~a : " f)
             f))
          f))
       po-files))

(define (find-origins needle haystacks)
  "TODO use parallel mapping
Searches for a needle in multiple, i.e. a list of, haystacks.

Returns a list since the needle, if it is a regexp can be defined on multiple places,
in a single haystacks and/or in multiple haystacks.

(define lst (list (cons \"a\" \"b\")
                  (cons \"c\" (list \"d\" \"e\" \"f\"))
                  (cons \"g\" \"h\")
                  (cons \"g\" \"h\")
                  (cons \"i\" (list \"j\" \"k\"))))
(find-origins \"h\" lst) ; = > (\"g\" \"g\") ;; like a regexp
(find-origins \"e\" lst) ; = > (\"c\")
(find-origins \"a\" lst) ; => ()
"
  ((compose
    (partial remove unspecified?)
    (partial map (partial find-in-cell-tail needle)))
   haystacks))

(define (translation-origins foreign-string-text po-file)
  "TODO use parallel mapping
Returns the original english version for a string by (reverse) lookup in the PO-FILE

(translation-origins \"Nom d'hôte\" \"/home/bost/dev/guix/po/guix/fr.po\")
; => (\"Hostname)
"
  (find-origins foreign-string-text
                (read-all-pofile po-file)))

;; display surrounding define

(define (find-sexps module-syntaxes origin)
  ((compose
    (partial remove unspecified?)
    (partial map (lambda (module-stx)
                   (let [(result (contains-sexp-syntax? module-stx origin))]
                     (when result
                       ;; This is wrong. I need particular element from the
                       ;; module-stx
                       (cons module-stx result))))))
   module-syntaxes))

(define (go)
  (let* [(foreign-string-text "Nom d'hôte")
         (po-file "/home/bost/dev/guix/po/guix/fr.po")
         (origins (translation-origins foreign-string-text po-file))
         (module-syntaxes (all-module-syntaxes))]
    ((compose
      (partial map syntax->datum)
      (partial map (lambda (sexps) (car sexps)))
      (partial map (lambda (sexps) (car sexps)))
      (partial map (lambda (sexps) (car sexps)))
      (partial map (partial find-sexps module-syntaxes))
      )
     origins)))
