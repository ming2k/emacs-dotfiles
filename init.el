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

;; Enable automatic reloading for all files
(global-auto-revert-mode 1)

;; Window navigation with Shift+arrow keys
(windmove-default-keybindings)

;; Unbind to disable middle click paste
(global-set-key [mouse-2] #'ignore)
;; Disable touchpad pinch scale text up and down
(global-set-key (kbd "<pinch>") 'ignore)

;; Disable middle-click (yank/paste)
(setq mouse-yank-at-point nil)

;; Enable mouse support in terminal
;;(unless (display-graphic-p) )

;; Add lisp directory to load-path
(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (when (file-directory-p lisp-dir)
    (add-to-list 'load-path lisp-dir)
    (dolist (category '("lang"))
      (let ((category-dir (expand-file-name category lisp-dir)))
        (when (file-directory-p category-dir)
          (add-to-list 'load-path category-dir))))))

;; Package management setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;;; Make sure the file name and "provide" name are consistent

;; Load core config
(require 'ming-editing)
(require 'ming-session)
(require 'ming-ui)

;; Load tool config
(require 'magit-setup)
(require 'org-setup)
(require 'web-dev)

;; Load language configurations
(require 'cc-setup)
(require 'python-setup)
(require 'rust-setup)
(require 'javascript-setup)
(require 'typescript-setup)
(require 'go-setup)
(require 'shell-setup)
(require 'lisp-setup)
(require 'emacs-lisp-setup)
(require 'yaml-setup)
(require 'markdown-setup)
(require 'zig-setup)
(require 'json-setup)
(require 'lua-setup)
(require 'justfile-setup)

;; Add a specific directory for themes to custom-theme-load-path
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
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
