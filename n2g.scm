(define-module (n2g)
  ;; #:use-module (utils)
  ;; #:export ()
  )


;; N
(define Nstd
  (list))

;; N'
;; https://search.nixos.org/
;; $ nix-env -qa 'a.*-theme' - ???
;; $ rg -A 6 '"a[a-z-]*-theme"' ~/dev/nixpkgs/pkgs/applications/editors/emac
(define Nnrm
  (list
   ;; https://search.nixos.org/packages?channel=23.11&from=0&size=50&sort=relevance&type=packages&query=emacsPackages.ample-theme+i686-linux+
   "emacsPackages.ample-theme"
   ))

;; G
;; $ guix package --list-available='^emacs-a.*-theme' | awk '{print "\""$1"\""}'
(define Gstd
  (list
   "emacs-ample-theme"

   ;; "emacs-abyss-theme"
   ;; "emacs-acme-theme"
   ;; "emacs-afternoon-theme"
   ;; "emacs-ahungry-theme"
   ;; "emacs-airline-themes"
   ;; "emacs-alect-themes"
   ;; "emacs-almost-mono-themes"
   ;; "emacs-ample-zen-theme"
   ;; "emacs-anti-zenburn-theme"
   ;; "emacs-apropospriate-theme"
   ;; "emacs-atom-one-dark-theme"
   ))

;; G'
(define Gnrm
  (list))
