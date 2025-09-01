;;; init.el --- Modern Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; A clean, modern Emacs configuration

;;; Code:

;; UTF-8 everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

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

;; Window navigation with Shift+arrow keys
(windmove-default-keybindings)

;; Enable mouse support in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4] 'scroll-down-line)
  (global-set-key [mouse-5] 'scroll-up-line))

;;; Session Management
;; Session management moved to config/core/session/session-config.el

;(setq trusted-content :all)
;(setq trusted-content '("~/.emacs.d/init.el"))

;; Simple keybindings
(global-set-key (kbd "C-c s") 'yas-insert-snippet)

;;; Load Config Modules

;; Add config directories to load-path
(let ((config-dir (expand-file-name "config" user-emacs-directory)))
  ;; Add all config subdirectories to load-path for flat structure
  (dolist (category '("core" "ui" "tools" "lang" "frameworks" "platform"))
    (let ((category-dir (expand-file-name category config-dir)))
      (when (file-directory-p category-dir)
        (add-to-list 'load-path category-dir)))))

;; Load core config
(require 'completion-frontend)
(require 'diagnostics)
(require 'editing)
(require 'desktop-config)

;; Load UI config
(require 'themes)
(require 'appearance)

;; Load tool config
(require 'magit)
(require 'which-key-config)
(require 'org-config)
(require 'org-roam-config)

;; Language config modules are loaded on-demand by their respective major modes

;; Platform-specific configurations
(require 'platform)

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
