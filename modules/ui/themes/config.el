;;; modules/ui/themes/config.el -*- lexical-binding: t; -*-

;; Font configuration
(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :family "JetBrainsMonoNL Nerd Font"
                      :height 130
                      :weight 'normal))

;; Doom themes
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Doom modeline
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding nil
        doom-modeline-indent-info nil
        doom-modeline-checker-simple-format t
        doom-modeline-vcs-max-length 12
        doom-modeline-env-version t))

;; Icons (optional)
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; Line numbers
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :config
  (setq display-line-numbers-width-start t))

;; Highlight current line
(use-package hl-line
  :ensure nil
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Rainbow mode for color visualization
(use-package rainbow-mode
  :ensure t
  :hook ((css-mode . rainbow-mode)
         (web-mode . rainbow-mode)
         (html-mode . rainbow-mode)
         (scss-mode . rainbow-mode)
         (sass-mode . rainbow-mode)
         (org-mode . rainbow-mode)))

;; Which-key - displays available keybindings in popup
(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :config
  ;; Basic which-key settings
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.05
        which-key-sort-order 'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-separator " → "
        which-key-prefix-prefix "+"
        which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))
  
  ;; Better popup appearance
  (setq which-key-show-prefix 'echo
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-allow-imprecise-window-fit nil
        which-key-max-description-length 32
        which-key-compute-remaps t
        which-key-use-C-h-commands t)
    
  ;; Performance optimization
  (setq which-key-lighter nil)
  
  :bind (("C-h K" . which-key-show-top-level)
         ("C-h M" . which-key-show-major-mode)))
