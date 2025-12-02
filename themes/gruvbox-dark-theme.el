;;; gruvbox-dark-theme.el --- Gruvbox dark theme -*- lexical-binding: t; -*-

;;; Commentary:
;; A dark theme inspired by the gruvbox colorscheme
;;
;; This theme follows a layered color definition approach for better maintainability
;; and semantic consistency:
;;
;; 1. BASE LAYER: Core background and foreground colors with tonal variations
;;    - Provides the foundation palette with subtle gradations
;;    - Includes main bg/fg, soft/hard variants, and intermediate tones
;;    - Example: gruvbox-dark-bg, gruvbox-dark-bg-soft, gruvbox-dark-bg-lighter
;;
;; 2. CHROMATIC LAYER: Pure color definitions in multiple intensities
;;    - Raw color values without semantic meaning
;;    - Each color has normal and soft variants for flexibility
;;    - Example: gruvbox-dark-red, gruvbox-dark-red-soft
;;
;; 3. SEMANTIC LAYER: Colors assigned to specific UI purposes
;;    - Links, visited links, cursor, search matches, etc.
;;    - Provides meaning and context to color usage
;;    - Example: gruvbox-dark-link, gruvbox-dark-cursor, gruvbox-dark-match
;;
;; 4. FUNCTIONAL LAYER: Face definitions using semantic and base colors
;;    - Maps semantic colors to actual UI elements
;;    - Ensures consistent visual hierarchy and accessibility
;;    - Example: `(cursor ((t (:background ,gruvbox-dark-cursor))))

;;; Code:

(deftheme gruvbox-dark
  "A dark theme inspired by gruvbox colorscheme")

(let (;; === BASE LAYER: Foundation colors with tonal variations ===
      (gruvbox-dark-bg        "#1e1e1e")
      (gruvbox-dark-bg-lighter "#3c3836")
      (gruvbox-dark-bg-soft   "#252420")
      (gruvbox-dark-bg-hard   "#0d1011")
      (gruvbox-dark-fg        "#ebdbb2")
      (gruvbox-dark-fg-dim    "#d5c4a1")
      (gruvbox-dark-fg-soft   "#a89984")
      (gruvbox-dark-gray      "#928374")
      (gruvbox-dark-gray-light "#a89984")
      (gruvbox-dark-comment   "#7c6f64")

      ;; === CHROMATIC LAYER: Pure color definitions ===
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
      (gruvbox-dark-orange-soft "#fe8019")

      ;; === SEMANTIC LAYER: Purpose-driven color assignments ===
      (gruvbox-dark-link      "#83a598")  ; Links (blue-soft)
      (gruvbox-dark-visited   "#d3869b")  ; Visited links (purple-soft)
      (gruvbox-dark-cursor    "#fe8019")  ; Cursor (orange-soft)
      (gruvbox-dark-match     "#d79921")) ; Search matches (yellow)

  ;; === FUNCTIONAL LAYER: Face definitions using layered colors ===
  (custom-theme-set-faces
   'gruvbox-dark

   ;; Basic faces
   `(default ((t (:background ,gruvbox-dark-bg :foreground ,gruvbox-dark-fg))))
   `(cursor ((t (:background ,gruvbox-dark-cursor))))
   `(region ((t (:background ,gruvbox-dark-bg-lighter))))
   `(highlight ((t (:background ,gruvbox-dark-bg-lighter))))
   `(hl-line ((t (:background ,gruvbox-dark-bg-soft))))
   `(fringe ((t (:background ,gruvbox-dark-bg))))
   `(mode-line ((t (:background ,gruvbox-dark-bg-soft :foreground ,gruvbox-dark-fg))))
   `(mode-line-inactive ((t (:background ,gruvbox-dark-bg-hard :foreground ,gruvbox-dark-fg-dim))))
   `(minibuffer-prompt ((t (:foreground ,gruvbox-dark-blue-soft :weight bold))))
   
   ;; Font lock faces
   `(font-lock-builtin-face ((t (:foreground ,gruvbox-dark-orange))))
   `(font-lock-comment-face ((t (:foreground ,gruvbox-dark-comment :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,gruvbox-dark-purple-soft))))
   `(font-lock-function-name-face ((t (:foreground ,gruvbox-dark-green-soft :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,gruvbox-dark-red :weight bold))))
   `(font-lock-string-face ((t (:foreground ,gruvbox-dark-green-soft))))
   `(font-lock-type-face ((t (:foreground ,gruvbox-dark-yellow-soft))))
   `(font-lock-variable-name-face ((t (:foreground ,gruvbox-dark-blue-soft))))
   `(font-lock-warning-face ((t (:foreground ,gruvbox-dark-red-soft :weight bold))))
   
   ;; Line numbers
   `(line-number ((t (:foreground ,gruvbox-dark-comment :background ,gruvbox-dark-bg))))
   `(line-number-current-line ((t (:foreground ,gruvbox-dark-yellow :background ,gruvbox-dark-bg-soft :weight bold))))

   ;; Fill column indicator
   `(fill-column-indicator ((t (:foreground ,gruvbox-dark-comment))))
   
   ;; Search
   `(isearch ((t (:background ,gruvbox-dark-match :foreground ,gruvbox-dark-bg))))
   `(lazy-highlight ((t (:background ,gruvbox-dark-bg-lighter :foreground ,gruvbox-dark-yellow))))
   
   ;; Org mode
   `(org-level-1 ((t (:foreground ,gruvbox-dark-blue-soft :weight bold))))
   `(org-level-2 ((t (:foreground ,gruvbox-dark-green-soft :weight bold))))
   `(org-level-3 ((t (:foreground ,gruvbox-dark-yellow :weight bold))))
   `(org-level-4 ((t (:foreground ,gruvbox-dark-purple-soft :weight bold))))
   `(org-code ((t (:foreground ,gruvbox-dark-orange :background ,gruvbox-dark-bg-soft))))
   `(org-verbatim ((t (:foreground ,gruvbox-dark-aqua-soft))))
   `(org-block ((t (:background ,gruvbox-dark-bg-soft))))
   `(org-block-begin-line ((t (:foreground ,gruvbox-dark-comment :background ,gruvbox-dark-bg-soft))))
   `(org-block-end-line ((t (:foreground ,gruvbox-dark-comment :background ,gruvbox-dark-bg-soft))))
   
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
   `(company-tooltip-selection ((t (:background ,gruvbox-dark-bg-lighter :foreground ,gruvbox-dark-fg))))
   `(company-tooltip-common ((t (:foreground ,gruvbox-dark-yellow :weight bold))))

   ;; Corfu completion faces
   `(corfu-default ((t (:background ,gruvbox-dark-bg-soft :foreground ,gruvbox-dark-fg))))
   `(corfu-current ((t (:background ,gruvbox-dark-bg-lighter :foreground ,gruvbox-dark-fg))))
   `(corfu-bar ((t (:background ,gruvbox-dark-gray))))
   `(corfu-border ((t (:background ,gruvbox-dark-gray))))
   
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
   `(vertico-current ((t (:background ,gruvbox-dark-bg-lighter))))
   
   ;; Marginalia
   `(marginalia-key ((t (:foreground ,gruvbox-dark-yellow))))
   `(marginalia-documentation ((t (:foreground ,gruvbox-dark-fg-dim))))
   
   ;; Error/warning faces
   `(error ((t (:foreground ,gruvbox-dark-red-soft :weight bold))))
   `(warning ((t (:foreground ,gruvbox-dark-yellow :weight bold))))
   `(success ((t (:foreground ,gruvbox-dark-green-soft :weight bold))))

   ;; Links
   `(link ((t (:foreground ,gruvbox-dark-link :underline t))))
   `(link-visited ((t (:foreground ,gruvbox-dark-visited :underline t))))

   ;; Secondary text using intermediate tones
   `(shadow ((t (:foreground ,gruvbox-dark-fg-dim))))
   `(secondary-selection ((t (:background ,gruvbox-dark-bg-lighter))))))

(provide-theme 'gruvbox-dark)

;;; gruvbox-dark-theme.el ends here