#!/usr/bin/env guile
!#

;;; From
;;; https://guix.gnu.org/manual/devel/en/guix.html#index-Git_002c-using-the-latest-commit
;;;
;;; Checkouts are kept in a cache under ~/.cache/guix/checkouts to speed up
;;; consecutive accesses to the same repository. You may want to clean it up
;;; once in a while to save disk space.

(use-modules
 ;; provides read-line
 (ice-9 rdelim)
 ;; provides open-input-pipe
 (ice-9 popen))

(define (read-all-strings port)
  "Return a list of all lines of text from the PORT.
Returns a list of strings."
  (let loop ((res '())
             (str (read-line port))) ; read-line from (ice-9 rdelim)
    (if (and str (not (eof-object? str)))
        (loop (append res (list str))
              (read-line port))
        res)))

(define-public (exec command)
  "Run the shell COMMAND using ‘/bin/sh -c’ with ‘OPEN_READ’ mode, ie. to read
from the subprocess. Wait for the command to terminate and return a string
containing its output."
  ;; (format #t "[DEBUG] command: ~a\n" command)
  (let* ((port (open-input-pipe command)) ; open-input-pipe from (ice-9 popen)
         ;; `read-all-strings' must be called before `close-pipe'.
         (results (read-all-strings port)))
    (close-pipe port)  ;; the return code of the command execution is ignored
    results))

(define checkoutCache (format #f "~a/.cache/guix/checkouts" (getenv "HOME")))

;; regular expression for matching an URL
(define regexp "\\(https\\|http\\|file\\)\\?://[^ ]\\+")
;; (format #t "[DEBUG] regexp: ~a\n" regexp)

(define cacheDirs
  (exec
   (format #f "find ~a -mindepth 1 -maxdepth 1 -type d" checkoutCache)))
;; (format #t "[DEBUG] ~a cacheDirs:\n" (length cacheDirs))
;; (format #t "~a\n" (string-join cacheDirs "\n"))

;;  find items under $checkoutCache which are NOT directories
(define cacheNonDirs
  (exec
   (format #f "find ~a -mindepth 1 -maxdepth 1 -not -type d" checkoutCache)))
;; (format #t "[DEBUG] ~a cacheNonDirs:\n" (length cacheNonDirs))
;; (format #t "~a\n" (string-join cacheNonDirs "\n"))

(define allChannelRepos
  (exec
   (format
    #f "guix system describe | grep repository | grep --only-matching \"~a\""
    regexp)))
;; (format #t "[DEBUG] ~a allChannelRepos:\n" (length allChannelRepos))
;; (format #t "~a\n" (string-join allChannelRepos "\n"))

;; define empty lists
(define keepDirs   (list)) ; local clones of git repositories actively used & cached
(define keepURLs   (list)) ; fetch-URLs of these clones
(define removeDirs (list)) ; inactive clones ...
(define removeURLs (list)) ; ... and theirs fetch-URLs
(define others     (list)) ; other, non-git-repository items in the checkout cache

(for-each
 (lambda (dir)
   ;; (format #t "[DEBUG] dir: ~a\n" dir)
   (define dirGit (format #f "~a/.git" dir))
   (if (access? dirGit F_OK) ;; check if file or directory exists
       (begin
         ;; (format #t "[DEBUG] #t (access? \"~a\" F_OK)\n" dirGit)
         (let ((dirURL
                (car ; `exec` returns a list of strings. Assume just one item in the list
                 (exec
                  (format
                   #f
                   "git --git-dir=~a/.git remote --verbose | grep fetch | grep --only-matching \"~a\""
                   dir regexp)))))
           ;; (format #t "[DEBUG] dirURL:\n~a\n" (string-join dirURL "\n"))
           (if (member dirURL allChannelRepos)
               (begin
                 ;; (format #t "[DEBUG] #t (member \"~a\" allChannelRepos)\n" dirURL)
                 (set! keepDirs (append keepDirs (list dir)))
                 (set! keepURLs (append keepURLs (list dirURL))))
               (begin
                 ;; (format #t "[DEBUG] #f (member \"~a\" allChannelRepos)\n" dirURL)
                 (set! removeDirs (append removeDirs (list dir)))
                 (set! removeURLs (append removeURLs (list dirURL)))))))
       (begin
         ;; (format #t "[DEBUG] #f (access? \"~a\" F_OK)\n" dirGit)
         (set! others (append others (list dir))))))
 cacheDirs)

;; `(iota N)` returns a list of numbers (0 ... N-1)
(if (> (length removeDirs) 0)
    (begin
      (format #t "### ~a Cached checkouts to delete:\n" (length removeDirs))
      (for-each
       (lambda (i)
         (let ((removeCheckoutsI (list-ref removeDirs i))
               (removeURLsI      (list-ref removeURLs i)))
           (format #t "mv ~a{,.deleteMe}  # ~a\n" removeCheckoutsI removeURLsI)))
       (iota (length removeDirs)))))

(if (> (length keepDirs) 0)
    (begin
      (format #t "\n### ~a Cached checkouts to keep:\n" (length keepDirs))
      (for-each
       (lambda (i)
         (let ((keepCheckoutsI (list-ref keepDirs i))
               (keepURLsI      (list-ref keepURLs i)))
           (format #t "# ~a  # ~a\n" keepCheckoutsI keepURLsI)))
       (iota (length keepDirs)))))

(if (> (length cacheNonDirs) 0)
    (begin
      (format #t "\n### ~a Non-directory items in the checkout cache directory:\n" (length cacheNonDirs))
      (for-each (lambda (elem) (format #t "# ~a\n" elem)) cacheNonDirs)))

(if (> (length others) 0)
    (begin
      (format #t "\n### ~a Other items in the checkout cache directory:\n" (length others))
      (for-each (lambda (elem) (format #t "# ~a\n" elem)) others)))
