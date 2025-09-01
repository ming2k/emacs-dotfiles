;;; dracula-theme.el --- Dracula theme -*- lexical-binding: t; -*-

;;; Commentary:
;; A dark theme inspired by the Dracula colorscheme

;;; Code:

(deftheme dracula
  "A dark theme inspired by Dracula colorscheme")

(let ((dracula-bg          "#282a36")
      (dracula-bg-soft     "#44475a")
      (dracula-bg-hard     "#21222c")
      (dracula-fg          "#f8f8f2")
      (dracula-fg-soft     "#6272a4")
      (dracula-gray        "#44475a")
      (dracula-red         "#ff5555")
      (dracula-green       "#50fa7b")
      (dracula-yellow      "#f1fa8c")
      (dracula-blue        "#8be9fd")
      (dracula-purple      "#bd93f9")
      (dracula-pink        "#ff79c6")
      (dracula-orange      "#ffb86c"))

  (custom-theme-set-faces
   'dracula
   
   ;; Basic faces
   `(default ((t (:background ,dracula-bg :foreground ,dracula-fg))))
   `(cursor ((t (:background ,dracula-pink))))
   `(region ((t (:background ,dracula-bg-soft))))
   `(highlight ((t (:background ,dracula-bg-soft))))
   `(hl-line ((t (:background ,dracula-bg-soft))))
   `(fringe ((t (:background ,dracula-bg))))
   `(mode-line ((t (:background ,dracula-bg-soft :foreground ,dracula-fg))))
   `(mode-line-inactive ((t (:background ,dracula-bg-hard :foreground ,dracula-fg-soft))))
   `(minibuffer-prompt ((t (:foreground ,dracula-purple :weight bold))))
   
   ;; Font lock faces
   `(font-lock-builtin-face ((t (:foreground ,dracula-purple))))
   `(font-lock-comment-face ((t (:foreground ,dracula-fg-soft :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,dracula-purple))))
   `(font-lock-function-name-face ((t (:foreground ,dracula-green :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,dracula-pink :weight bold))))
   `(font-lock-string-face ((t (:foreground ,dracula-yellow))))
   `(font-lock-type-face ((t (:foreground ,dracula-blue))))
   `(font-lock-variable-name-face ((t (:foreground ,dracula-orange))))
   `(font-lock-warning-face ((t (:foreground ,dracula-red :weight bold))))
   
   ;; Line numbers
   `(line-number ((t (:foreground ,dracula-fg-soft :background ,dracula-bg))))
   `(line-number-current-line ((t (:foreground ,dracula-yellow :background ,dracula-bg-soft :weight bold))))
   
   ;; Search
   `(isearch ((t (:background ,dracula-orange :foreground ,dracula-bg))))
   `(lazy-highlight ((t (:background ,dracula-yellow :foreground ,dracula-bg))))
   
   ;; Org mode
   `(org-level-1 ((t (:foreground ,dracula-purple :weight bold))))
   `(org-level-2 ((t (:foreground ,dracula-blue :weight bold))))
   `(org-level-3 ((t (:foreground ,dracula-green :weight bold))))
   `(org-level-4 ((t (:foreground ,dracula-yellow :weight bold))))
   `(org-code ((t (:foreground ,dracula-orange :background ,dracula-bg-soft))))
   `(org-verbatim ((t (:foreground ,dracula-green))))
   `(org-block ((t (:background ,dracula-bg-soft))))
   `(org-block-begin-line ((t (:foreground ,dracula-fg-soft :background ,dracula-bg-soft))))
   `(org-block-end-line ((t (:foreground ,dracula-fg-soft :background ,dracula-bg-soft))))
   
   ;; Dired
   `(dired-directory ((t (:foreground ,dracula-blue :weight bold))))
   `(dired-symlink ((t (:foreground ,dracula-pink))))
   
   ;; Magit
   `(magit-branch-local ((t (:foreground ,dracula-purple))))
   `(magit-branch-remote ((t (:foreground ,dracula-green))))
   `(magit-diff-added ((t (:background ,dracula-green :foreground ,dracula-bg))))
   `(magit-diff-removed ((t (:background ,dracula-red :foreground ,dracula-bg))))
   `(magit-section-heading ((t (:foreground ,dracula-yellow :weight bold))))
   
   ;; Company/Corfu completion
   `(company-tooltip ((t (:background ,dracula-bg-soft :foreground ,dracula-fg))))
   `(company-tooltip-selection ((t (:background ,dracula-purple :foreground ,dracula-bg))))
   `(company-tooltip-common ((t (:foreground ,dracula-yellow :weight bold))))
   
   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,dracula-red))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,dracula-orange))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,dracula-yellow))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,dracula-green))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,dracula-blue))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,dracula-purple))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,dracula-pink))))
   
   ;; Which-key
   `(which-key-key-face ((t (:foreground ,dracula-blue :weight bold))))
   `(which-key-description-face ((t (:foreground ,dracula-fg))))
   `(which-key-group-description-face ((t (:foreground ,dracula-purple))))
   `(which-key-command-description-face ((t (:foreground ,dracula-green))))
   
   ;; Vertico
   `(vertico-current ((t (:background ,dracula-bg-soft))))
   
   ;; Marginalia
   `(marginalia-key ((t (:foreground ,dracula-blue))))
   `(marginalia-documentation ((t (:foreground ,dracula-fg-soft))))
   
   ;; Error/warning faces
   `(error ((t (:foreground ,dracula-red :weight bold))))
   `(warning ((t (:foreground ,dracula-orange :weight bold))))
   `(success ((t (:foreground ,dracula-green :weight bold))))))

(provide-theme 'dracula)

;;; dracula-theme.el ends here