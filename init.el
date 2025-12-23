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

;; Authentication sources - used by mu4e, sieve-manage, TRAMP, ERC, etc.
;; Supports both encrypted .authinfo.gpg (recommended) and plain .authinfo
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))

;; Enable automatic reloading for all files
(global-auto-revert-mode 1)

;; Window navigation with Shift+arrow keys
(windmove-default-keybindings)

;; Winner mode - undo/redo window configuration changes
(winner-mode 1)

;; Unbind to disable middle click paste
(global-set-key [mouse-2] #'ignore)
;; Disable touchpad pinch scale text up and down
(global-set-key (kbd "<pinch>") 'ignore)

;; Disable middle-click (yank/paste)
(setq mouse-yank-at-point nil)

;; Enable mouse support in terminal
;;(unless (display-graphic-p) )

;; Package management setup
(require 'package)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure nil) ; We use :ensure t explicitly per package

;; Custom file for Emacs-generated code
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Add lisp directory to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;;; Make sure the file name and "provide" name are consistent

;; Load core config
(require 'init-completion)
(require 'init-lsp)
(require 'init-editing)
(require 'init-session)
(require 'init-project)
(require 'init-ui)

;; Load tool config
(require 'init-git)
(require 'init-org)
(require 'init-web)
(require 'init-mail)
(require 'init-elfeed)
(require 'init-erc)
(require 'init-eww)

;; Add a specific directory for themes to custom-theme-load-path
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
;; Disable all themes before loading to avoid conflicts on reload
(mapc #'disable-theme custom-enabled-themes)
;; Load the theme using the standard function
(load-theme 'gruvbox-dark t)

;; Essential settings
(setq select-enable-clipboard t
      select-enable-primary nil) ; `primary` uses different clipboard mechanism with `clipboard`

;; Clipboard for linux
(defun wl-copy (text)
  (let ((process-connection-type nil))
    (let ((proc (start-process "wl-copy" "*Messages*" "wl-copy" "-f" "-n")))
      (process-send-string proc text)
      (process-send-eof proc))))
(defun wl-paste ()
  (shell-command-to-string "wl-paste -n"))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

;; Performance optimizations
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq inhibit-compacting-font-caches t)

(provide 'init)
;;; init.el ends here
