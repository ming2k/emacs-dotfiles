;;; gruvbox-dark-theme.el --- Gruvbox dark theme -*- lexical-binding: t; -*-

;;; Commentary:
;; A dark theme inspired by the gruvbox colorscheme

;;; Code:

(deftheme gruvbox-dark
  "A dark theme inspired by gruvbox colorscheme")

(let ((gruvbox-dark-bg        "#282828")
      (gruvbox-dark-bg-soft   "#32302f")
      (gruvbox-dark-bg-hard   "#1d2021")
      (gruvbox-dark-fg        "#ebdbb2")
      (gruvbox-dark-fg-soft   "#a89984")
      (gruvbox-dark-gray      "#928374")
      (gruvbox-dark-red       "#cc241d")
      (gruvbox-dark-red-soft  "#fb4934")
      (gruvbox-dark-green     "#98971a")
      (gruvbox-dark-green-soft "#b8bb26")
      (gruvbox-dark-yellow    "#d79921")
      (gruvbox-dark-yellow-soft "#fabd2f")
      (gruvbox-dark-blue      "#458588")
      (gruvbox-dark-blue-soft "#83a598")
      (gruvbox-dark-purple    "#b16286")
      (gruvbox-dark-purple-soft "#d3869b")
      (gruvbox-dark-aqua      "#689d6a")
      (gruvbox-dark-aqua-soft "#8ec07c")
      (gruvbox-dark-orange    "#d65d0e")
      (gruvbox-dark-orange-soft "#fe8019"))

  (custom-theme-set-faces
   'gruvbox-dark
   
   ;; Basic faces
   `(default ((t (:background ,gruvbox-dark-bg :foreground ,gruvbox-dark-fg))))
   `(cursor ((t (:background ,gruvbox-dark-orange))))
   `(region ((t (:background ,gruvbox-dark-aqua))))
   `(highlight ((t (:background ,gruvbox-dark-bg-soft))))
   `(hl-line ((t (:background ,gruvbox-dark-bg-soft))))
   `(fringe ((t (:background ,gruvbox-dark-bg))))
   `(mode-line ((t (:background ,gruvbox-dark-bg-soft :foreground ,gruvbox-dark-fg))))
   `(mode-line-inactive ((t (:background ,gruvbox-dark-bg-hard :foreground ,gruvbox-dark-gray))))
   `(minibuffer-prompt ((t (:foreground ,gruvbox-dark-blue-soft :weight bold))))
   
   ;; Font lock faces
   `(font-lock-builtin-face ((t (:foreground ,gruvbox-dark-orange))))
   `(font-lock-comment-face ((t (:foreground ,gruvbox-dark-gray :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,gruvbox-dark-purple))))
   `(font-lock-function-name-face ((t (:foreground ,gruvbox-dark-green-soft :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,gruvbox-dark-red :weight bold))))
   `(font-lock-string-face ((t (:foreground ,gruvbox-dark-green))))
   `(font-lock-type-face ((t (:foreground ,gruvbox-dark-yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,gruvbox-dark-blue))))
   `(font-lock-warning-face ((t (:foreground ,gruvbox-dark-red-soft :weight bold))))
   
   ;; Line numbers
   `(line-number ((t (:foreground ,gruvbox-dark-gray :background ,gruvbox-dark-bg))))
   `(line-number-current-line ((t (:foreground ,gruvbox-dark-yellow :background ,gruvbox-dark-bg-soft :weight bold))))
   
   ;; Search
   `(isearch ((t (:background ,gruvbox-dark-yellow :foreground ,gruvbox-dark-bg))))
   `(lazy-highlight ((t (:background ,gruvbox-dark-yellow-soft :foreground ,gruvbox-dark-bg))))
   
   ;; Org mode
   `(org-level-1 ((t (:foreground ,gruvbox-dark-blue-soft :weight bold))))
   `(org-level-2 ((t (:foreground ,gruvbox-dark-green-soft :weight bold))))
   `(org-level-3 ((t (:foreground ,gruvbox-dark-yellow :weight bold))))
   `(org-level-4 ((t (:foreground ,gruvbox-dark-purple-soft :weight bold))))
   `(org-code ((t (:foreground ,gruvbox-dark-orange :background ,gruvbox-dark-bg-soft))))
   `(org-verbatim ((t (:foreground ,gruvbox-dark-green))))
   `(org-block ((t (:background ,gruvbox-dark-bg-soft))))
   `(org-block-begin-line ((t (:foreground ,gruvbox-dark-gray :background ,gruvbox-dark-bg-soft))))
   `(org-block-end-line ((t (:foreground ,gruvbox-dark-gray :background ,gruvbox-dark-bg-soft))))
   
   ;; Dired
   `(dired-directory ((t (:foreground ,gruvbox-dark-blue-soft :weight bold))))
   `(dired-symlink ((t (:foreground ,gruvbox-dark-aqua))))
   
   ;; Magit
   `(magit-branch-local ((t (:foreground ,gruvbox-dark-blue-soft))))
   `(magit-branch-remote ((t (:foreground ,gruvbox-dark-green))))
   `(magit-diff-added ((t (:background ,gruvbox-dark-green :foreground ,gruvbox-dark-bg))))
   `(magit-diff-removed ((t (:background ,gruvbox-dark-red :foreground ,gruvbox-dark-bg))))
   `(magit-section-heading ((t (:foreground ,gruvbox-dark-yellow :weight bold))))
   
   ;; Company/Corfu completion
   `(company-tooltip ((t (:background ,gruvbox-dark-bg-soft :foreground ,gruvbox-dark-fg))))
   `(company-tooltip-selection ((t (:background ,gruvbox-dark-blue :foreground ,gruvbox-dark-bg))))
   `(company-tooltip-common ((t (:foreground ,gruvbox-dark-yellow :weight bold))))
   
   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,gruvbox-dark-red-soft))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,gruvbox-dark-green-soft))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,gruvbox-dark-yellow-soft))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,gruvbox-dark-blue-soft))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,gruvbox-dark-purple-soft))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,gruvbox-dark-aqua-soft))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,gruvbox-dark-orange-soft))))
   
   ;; Which-key
   `(which-key-key-face ((t (:foreground ,gruvbox-dark-yellow :weight bold))))
   `(which-key-description-face ((t (:foreground ,gruvbox-dark-fg))))
   `(which-key-group-description-face ((t (:foreground ,gruvbox-dark-blue-soft))))
   `(which-key-command-description-face ((t (:foreground ,gruvbox-dark-green))))
   
   ;; Vertico
   `(vertico-current ((t (:background ,gruvbox-dark-bg-soft))))
   
   ;; Marginalia
   `(marginalia-key ((t (:foreground ,gruvbox-dark-yellow))))
   `(marginalia-documentation ((t (:foreground ,gruvbox-dark-gray))))
   
   ;; Error/warning faces
   `(error ((t (:foreground ,gruvbox-dark-red-soft :weight bold))))
   `(warning ((t (:foreground ,gruvbox-dark-yellow :weight bold))))
   `(success ((t (:foreground ,gruvbox-dark-green-soft :weight bold))))))

(provide-theme 'gruvbox-dark)

;;; gruvbox-dark-theme.el ends here