;;; early-init.el --- Early initialization configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is loaded before the package system and GUI is initialized.
;; It's the ideal place for package archives setup and early optimizations.

;;; Code:

;; -----------------------------------------------------------------------------
;; Basic UI Framework and Style
;; -----------------------------------------------------------------------------

(set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 120)

;; Get straight to a clean editor when starting Emacs
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 40))
;; Set all new frames to be created without decorations by default
(add-to-list 'default-frame-alist '(undecorated . t))
;; Set temporary colors to avoid white flash on startup (theme will override)
(add-to-list 'default-frame-alist '(background-color . "#1e1e1e"))
(add-to-list 'default-frame-alist '(foreground-color . "#ebdbb2"))

;; Disable UI elements early to avoid flashing
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Disable auto save feature
(setq auto-save-default nil)
(setq make-backup-files nil)

;; -----------------------------------------------------------------------------
;; Package Manager
;; -----------------------------------------------------------------------------

;; Prevent package.el from modifying init.el
(setq package-enable-at-startup nil)

;; Add package archives
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Add lisp directory to load-path early
(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (when (file-directory-p lisp-dir)
    (add-to-list 'load-path lisp-dir)
    (dolist (category '("lang"))
      (let ((category-dir (expand-file-name category lisp-dir)))
        (when (file-directory-p category-dir)
          (add-to-list 'load-path category-dir))))))

;; -----------------------------------------------------------------------------
;; Performance
;; -----------------------------------------------------------------------------

;; Performance optimizations
(setq read-process-output-max (* 1024 1024 2)) ; 2MB for LSP
(setq process-adaptive-read-buffering nil)

;; Optimize file-name-handler during startup
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Increase garbage collection threshold during startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Restore settings after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216) ; 16MB
            (setq gc-cons-percentage 0.1)
            (setq file-name-handler-alist file-name-handler-alist-original)))

(provide 'early-init)
;;; early-init.el ends here
