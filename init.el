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
;; Session management moved to modules/core/session/session-config.el

;(setq trusted-content :all)
;(setq trusted-content '("~/.emacs.d/init.el"))

;; Simple keybindings
(global-set-key (kbd "C-c s") 'yas-insert-snippet)

;;; Load Modules

;; Add module directories to load-path
(let ((modules-dir (expand-file-name "modules" user-emacs-directory)))
  ;; Add the core modules directory itself for standalone files like lsp.el
  (add-to-list 'load-path (expand-file-name "core" modules-dir))
  (dolist (category '("core" "ui" "tools" "lang"))
    (let ((category-dir (expand-file-name category modules-dir)))
      (when (file-directory-p category-dir)
        (dolist (module-dir (directory-files category-dir t "^[^.]"))
          (when (file-directory-p module-dir)
            (add-to-list 'load-path module-dir)))))))

;; Load core modules
(require 'completion-config)
(require 'diagnostics-config)
(require 'project-config)
(require 'editing-config)
(require 'session-config)

;; Load UI modules
(require 'themes-config)
(require 'appearance-config)
(require 'help-config)

;; Load tool modules
(require 'magit-config)
(require 'org-config)

;; Load language modules on-demand
(require 'lang)

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

;; Keep custom settings separate from configuration
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
