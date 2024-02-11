;; Α α, Β β, Γ γ, Δ δ, Ε ε, Ζ ζ, Η η, Θ θ, Ι ι, Κ κ, Λ λ, Μ μ, Ν ν, Ξ ξ, Ο ο, Π π, Ρ ρ, Σ σ/ς, Τ τ, Υ υ, Φ φ, Χ χ, Ψ ψ, Ω ω.

(defvar overlays (list)
  "Overlays for changing background of visible text.")

(defun overlay-line (position-center win-width left-clr right-clr)
  (let* ((horizontal-scroll (window-hscroll))
         (extra-decrease 0)
         (position-point (point))
         (position-end-of-line (pos-eol))
         (left-block-end (min (+ position-point position-center)
                              position-end-of-line))
         (left-block-begin (+ position-point horizontal-scroll extra-decrease))
         (left-block-width (- left-block-end left-block-begin)))
    (when (> left-block-width 0)
      (let ((o (make-overlay left-block-begin left-block-end)))
        (add-to-list 'overlays o)
        (overlay-put o 'face `(:background ,left-clr))))

    (let* ((right-block-begin left-block-end)
           (right-block-end (min position-end-of-line
                                 (+ left-block-begin win-width)))
           (right-block-width (- right-block-end right-block-begin)))
      (when (> right-block-width 0)
        (let ((o (make-overlay right-block-begin right-block-end)))
          (add-to-list 'overlays o)
          (overlay-put o 'face `(:background ,right-clr)))))))

(defun visible-quadrants ()
  "Update the background of visible text to a specific color."
  (interactive)
  ;; (window-point) - position of the point in current window
  (when (boundp 'overlays) (mapcar #'delete-overlay overlays))
  (setq overlays (list))

  (let* (
         (color--top-left "light coral")
         (color--top-right "light salmon")

         (color--bottom-left "yellow green")
         (color--bottom-right "deep sky blue")

         (color--center-left "light sea green")
         (color--center-right "medium aquamarine")

         (win-height (window-height))
         (curr-line (current-line))
         (win-width (window-width))
         ;; pos-bol "position begin of line"
         (position-center (- (point) (pos-bol))))
    (save-excursion ; Preserve the original position of the point
      (goto-char (window-start))
      (let* ((iterations-top (- curr-line (current-line))))
        (dotimes (i iterations-top)
          (overlay-line position-center win-width
                        color--top-left color--top-right)
          (next-line))

        (overlay-line position-center win-width
                      color--center-left color--center-right)
        (next-line)

        (let* ((iterations-bottom (- win-height iterations-top 1)))
          (dotimes (i iterations-bottom)
            (overlay-line position-center win-width
                          color--bottom-left color--bottom-right)
            (next-line)))))))

;; (add-hook 'window-scroll-functions 'update-visible-text-background)
