;;; gruvbox-light-theme.el --- Gruvbox light theme -*- lexical-binding: t; -*-

;;; Commentary:
;; A light theme inspired by the gruvbox colorscheme

;;; Code:

(deftheme gruvbox-light
  "A light theme inspired by gruvbox colorscheme")

(let ((gruvbox-light-bg        "#fbf1c7")
      (gruvbox-light-bg-soft   "#f2e5bc")
      (gruvbox-light-bg-hard   "#f9f5d7")
      (gruvbox-light-fg        "#3c3836")
      (gruvbox-light-fg-soft   "#504945")
      (gruvbox-light-gray      "#928374")
      (gruvbox-light-red       "#cc241d")
      (gruvbox-light-red-soft  "#9d0006")
      (gruvbox-light-green     "#98971a")
      (gruvbox-light-green-soft "#79740e")
      (gruvbox-light-yellow    "#d79921")
      (gruvbox-light-yellow-soft "#b57614")
      (gruvbox-light-blue      "#458588")
      (gruvbox-light-blue-soft "#076678")
      (gruvbox-light-purple    "#b16286")
      (gruvbox-light-purple-soft "#8f3f71")
      (gruvbox-light-aqua      "#689d6a")
      (gruvbox-light-aqua-soft "#427b58")
      (gruvbox-light-orange    "#d65d0e")
      (gruvbox-light-orange-soft "#af3a03"))

  (custom-theme-set-faces
   'gruvbox-light
   
   ;; Basic faces
   `(default ((t (:background ,gruvbox-light-bg :foreground ,gruvbox-light-fg))))
   `(cursor ((t (:background ,gruvbox-light-orange))))
   `(region ((t (:background ,gruvbox-light-bg-soft))))
   `(highlight ((t (:background ,gruvbox-light-bg-soft))))
   `(hl-line ((t (:background ,gruvbox-light-bg-soft))))
   `(fringe ((t (:background ,gruvbox-light-bg))))
   `(mode-line ((t (:background ,gruvbox-light-bg-soft :foreground ,gruvbox-light-fg))))
   `(mode-line-inactive ((t (:background ,gruvbox-light-bg-hard :foreground ,gruvbox-light-gray))))
   `(minibuffer-prompt ((t (:foreground ,gruvbox-light-blue-soft :weight bold))))
   
   ;; Font lock faces
   `(font-lock-builtin-face ((t (:foreground ,gruvbox-light-orange))))
   `(font-lock-comment-face ((t (:foreground ,gruvbox-light-gray :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,gruvbox-light-purple))))
   `(font-lock-function-name-face ((t (:foreground ,gruvbox-light-green-soft :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,gruvbox-light-red :weight bold))))
   `(font-lock-string-face ((t (:foreground ,gruvbox-light-green))))
   `(font-lock-type-face ((t (:foreground ,gruvbox-light-yellow-soft))))
   `(font-lock-variable-name-face ((t (:foreground ,gruvbox-light-blue))))
   `(font-lock-warning-face ((t (:foreground ,gruvbox-light-red-soft :weight bold))))
   
   ;; Line numbers
   `(line-number ((t (:foreground ,gruvbox-light-gray :background ,gruvbox-light-bg))))
   `(line-number-current-line ((t (:foreground ,gruvbox-light-yellow-soft :background ,gruvbox-light-bg-soft :weight bold))))
   
   ;; Search
   `(isearch ((t (:background ,gruvbox-light-yellow :foreground ,gruvbox-light-bg))))
   `(lazy-highlight ((t (:background ,gruvbox-light-yellow-soft :foreground ,gruvbox-light-bg))))
   
   ;; Org mode
   `(org-level-1 ((t (:foreground ,gruvbox-light-blue-soft :weight bold))))
   `(org-level-2 ((t (:foreground ,gruvbox-light-green-soft :weight bold))))
   `(org-level-3 ((t (:foreground ,gruvbox-light-yellow-soft :weight bold))))
   `(org-level-4 ((t (:foreground ,gruvbox-light-purple-soft :weight bold))))
   `(org-code ((t (:foreground ,gruvbox-light-orange :background ,gruvbox-light-bg-soft))))
   `(org-verbatim ((t (:foreground ,gruvbox-light-green))))
   `(org-block ((t (:background ,gruvbox-light-bg-soft))))
   `(org-block-begin-line ((t (:foreground ,gruvbox-light-gray :background ,gruvbox-light-bg-soft))))
   `(org-block-end-line ((t (:foreground ,gruvbox-light-gray :background ,gruvbox-light-bg-soft))))
   
   ;; Dired
   `(dired-directory ((t (:foreground ,gruvbox-light-blue-soft :weight bold))))
   `(dired-symlink ((t (:foreground ,gruvbox-light-aqua))))
   
   ;; Magit
   `(magit-branch-local ((t (:foreground ,gruvbox-light-blue-soft))))
   `(magit-branch-remote ((t (:foreground ,gruvbox-light-green))))
   `(magit-diff-added ((t (:background ,gruvbox-light-green :foreground ,gruvbox-light-bg))))
   `(magit-diff-removed ((t (:background ,gruvbox-light-red :foreground ,gruvbox-light-bg))))
   `(magit-section-heading ((t (:foreground ,gruvbox-light-yellow-soft :weight bold))))
   
   ;; Company/Corfu completion
   `(company-tooltip ((t (:background ,gruvbox-light-bg-soft :foreground ,gruvbox-light-fg))))
   `(company-tooltip-selection ((t (:background ,gruvbox-light-blue :foreground ,gruvbox-light-bg))))
   `(company-tooltip-common ((t (:foreground ,gruvbox-light-yellow-soft :weight bold))))
   
   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,gruvbox-light-red-soft))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,gruvbox-light-green-soft))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,gruvbox-light-yellow-soft))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,gruvbox-light-blue-soft))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,gruvbox-light-purple-soft))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,gruvbox-light-aqua-soft))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,gruvbox-light-orange-soft))))
   
   ;; Which-key
   `(which-key-key-face ((t (:foreground ,gruvbox-light-yellow-soft :weight bold))))
   `(which-key-description-face ((t (:foreground ,gruvbox-light-fg))))
   `(which-key-group-description-face ((t (:foreground ,gruvbox-light-blue-soft))))
   `(which-key-command-description-face ((t (:foreground ,gruvbox-light-green))))
   
   ;; Vertico
   `(vertico-current ((t (:background ,gruvbox-light-bg-soft))))
   
   ;; Marginalia
   `(marginalia-key ((t (:foreground ,gruvbox-light-yellow-soft))))
   `(marginalia-documentation ((t (:foreground ,gruvbox-light-gray))))
   
   ;; Error/warning faces
   `(error ((t (:foreground ,gruvbox-light-red-soft :weight bold))))
   `(warning ((t (:foreground ,gruvbox-light-yellow-soft :weight bold))))
   `(success ((t (:foreground ,gruvbox-light-green-soft :weight bold))))))

(provide-theme 'gruvbox-light)

;;; gruvbox-light-theme.el ends here