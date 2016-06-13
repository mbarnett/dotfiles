
(setq solarized-height-minus-1 1)
(setq solarized-height-plus-1 1)
(setq solarized-height-plus-2 1)
(setq solarized-height-plus-3 1)
(setq solarized-height-plus-4 1)
(setq x-underline-at-descent-line t)
(setq solarized-emphasize-indicators nil)
(setq solarized-use-variable-pitch nil)
(setq solarized-use-less-bold t)

(load-theme 'solarized-light t)

;; (defun unfuck-solarize-faces ()
;;   (set-face-attribute 'font-lock-warning-face nil :foreground "#d33682")
;;   (set-face-attribute 'font-lock-constant-face nil :bold nil)
;;   (set-face-attribute 'region nil :background "#094554" :foreground nil)
;;   (set-face-attribute 'show-paren-match nil :foreground nil :background "#004B5F"))

(with-eval-after-load "company"
  (set-face-attribute 'company-tooltip-common nil :underline nil :bold t)
  (set-face-attribute 'company-tooltip-common-selection nil :underline nil :bold t)
  (set-face-attribute 'company-preview-common nil :underline nil))

(with-eval-after-load "helm"
  (set-face-attribute 'helm-selection nil :underline nil)
  (set-face-attribute 'helm-source-header nil))

;; todo fix how it uses these, it doesn't use enough colors
(with-eval-after-load "rainbow-delimiters"
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "#d33682")
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :background "#dc322f" :foreground nil :inverse-video nil))

;; solarized clobers some of the faces when a new frame initializes, so we need to re-unfuck things
;; (add-hook 'after-make-frame-functions (lambda (frame)
;;                                         (unfuck-solarize-faces)))

;; (unfuck-solarize-faces)

(provide 'unfucked-solarize)
