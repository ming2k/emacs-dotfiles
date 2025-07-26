;; =============================================================================
;; config/ui.el - UI and theme configuration
;; =============================================================================
;;; Commentary:
;; UI configuration and theming
;;; Code:

;; Font configuration
(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :family "JetBrainsMonoNL Nerd Font"
                      :height 130
                      :weight 'normal))

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Modeline
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

;; Icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; Line numbers
(use-package display-line-numbers
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :config
  (setq display-line-numbers-width-start t))

;; Highlight current line
(use-package hl-line
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Which-key
; (use-package which-key
;   :ensure t
;   :init
;   (which-key-mode)
;   :config
;   (setq which-key-idle-delay 0.3
;         which-key-prefix-prefix "◉ "
;         which-key-sort-order 'which-key-key-order-alpha
;         which-key-min-display-lines 3
;         which-key-max-display-columns nil))

(provide 'ui)
;;; ui.el ends here
