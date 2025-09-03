;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; A clean, modern Emacs configuration

;;; Code:

;; UTF-8 everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(global-visual-line-mode 1)

;; Better defaults
(setq-default
 x-super-keysym nil
 indent-tabs-mode nil
 tab-width 4
 require-final-newline t
 sentence-end-double-space nil)

;; Auto-save and backup settings
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 3
      kept-old-versions 2
      version-control t)

;; Enable useful features
(electric-pair-mode -1)
(show-paren-mode 1)
(global-auto-revert-mode 1)
(delete-selection-mode 1)
(global-font-lock-mode 1)

;; Disable flyspell-mode by default
(setq-default flyspell-mode nil)

;; Window navigation with Shift+arrow keys
(windmove-default-keybindings)

;; Enable mouse support in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4] 'scroll-down-line)
  (global-set-key [mouse-5] 'scroll-up-line))

;;; Session Management
;; Session management moved to config/core/session/session-config.el

;;; Load Config Modules

;; Add config directories to load-path
(let ((config-dir (expand-file-name "config" user-emacs-directory)))
  ;; Add all config subdirectories to load-path for flat structure
  (dolist (category '("core" "tools" "lang" "platform"))
    (let ((category-dir (expand-file-name category config-dir)))
      (when (file-directory-p category-dir)
        (add-to-list 'load-path category-dir)))))

;; Load core config
(require 'completion-frontend)
(require 'diagnostics)
(require 'editing)
(require 'desktop-config)
(require 'appearance)
(require 'lsp-config)

;; Load tool config
(require 'magit)
(require 'which-key-config)
(require 'org-config)
(require 'org-roam-config)
(require 'svelte-config)

;; Load language configurations
(require 'cc-config)
(require 'python-config)
(require 'rust-config)
(require 'javascript-config)
(require 'typescript-config)
(require 'go-config)
(require 'shell-config)
(require 'lisp-config)
(require 'json-config)
(require 'markdown-config)
(require 'zig-config)

;; Platform-specific configurations
(require 'platform)

;; Add a specific directory for themes to custom-theme-load-path
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
;; Load the theme using the standard function
(load-theme 'gruvbox-dark t)

;; Essential settings
(setq select-enable-clipboard t
      select-enable-primary t)

;; Performance optimizations
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq inhibit-compacting-font-caches t)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
