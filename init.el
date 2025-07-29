;;; init.el --- Modern Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; A clean, modern Emacs configuration
;;; Code:

;; Package system is initialized in early-init.el
(require 'package)

;; Additional use-package optimizations  
(setq use-package-expand-minimally t
      use-package-compute-statistics t)

;;; Core Settings
;; UTF-8 everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Better defaults
(setq-default
 indent-tabs-mode nil
 tab-width 4
 fill-column 80
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

;;; Session Management
(use-package saveplace
  :ensure nil
  :config
  (setq save-place-limit 1000
        save-place-forget-unreadable-files t)
  (save-place-mode 1))

(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 100)
  (recentf-mode 1))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1)
  :config
  (setq savehist-length 1000
        savehist-additional-variables '(search-ring regexp-search-ring
                                       extended-command-history
                                       kill-ring)))

;;; Programming
;; Tree-sitter (built-in in Emacs 29+)
(when (treesit-available-p)
  (setq treesit-font-lock-level 4))

;; Flycheck for syntax checking
(use-package flycheck
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe))

;;; Editing Enhancements
;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Expand region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Smartparens
(use-package smartparens
  :ensure t
  :init
  ;; Load smartparens-config after smartparens is loaded
  (with-eval-after-load 'smartparens
    (require 'smartparens-config))
  :config
  (smartparens-global-mode)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil))

;; Better movement
(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

;; YASnippet
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Simple keybindings
(global-set-key (kbd "C-c s") 'yas-insert-snippet)

;;; Load Modules
(defun load-config-module (category module)
  "Load a configuration module."
  (let ((config-file (expand-file-name
                      (format "modules/%s/%s/config.el" category module)
                      user-emacs-directory)))
    (when (file-exists-p config-file)
      (load config-file))))

;; Load core modules
(load-config-module "core" "completion")
(load-config-module "core" "project")
(load-config-module "core" "editing")

;; Load UI modules
(load-config-module "ui" "themes")

;; Load tool modules  
(load-config-module "tools" "magit")
(load-config-module "tools" "org")

;; Load language modules
(load-config-module "lang" "cc")
(load-config-module "lang" "python") 
(load-config-module "lang" "rust")
(load-config-module "lang" "javascript")
(load-config-module "lang" "svelte")
(load-config-module "lang" "go")
(load-config-module "lang" "lisp")
(load-config-module "lang" "markdown")

;; System-specific configurations
(let ((system-config 
       (expand-file-name 
        (format "modules/system/%s.el" 
                (pcase system-type
                  ('darwin "macos")
                  ('gnu/linux "linux")
                  ('windows-nt "windows")))
        user-emacs-directory)))
  (when (file-exists-p system-config)
    (load system-config)))

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
