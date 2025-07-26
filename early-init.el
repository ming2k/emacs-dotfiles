;;; early-init.el --- Early initialization configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is loaded before the package system and GUI is initialized.
;; It's the ideal place for package archives setup and early optimizations.

;;; Code:

;; Load XDG path configuration before anything else
(add-to-list 'load-path (expand-file-name "config/system" user-emacs-directory))
(require 'xdg-paths)

;; Disable auto save feature
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Increase garbage collection threshold during startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Package system initialization
(require 'package)

;; Add package archives
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Initialize package system
(package-initialize)

;; Refresh package contents if needed
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Configure use-package
(eval-when-compile
  (require 'use-package))

;; Make use-package verbose during startup (optional)
(setq use-package-verbose t)
(setq use-package-always-ensure t) ; Automatically install packages

;; Disable UI elements early to avoid flashing
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Disable startup screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Disable file dialog boxes
(setq use-dialog-box nil)

;; Disable ring bell
(setq ring-bell-function 'ignore)

;; Frame settings
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)

;; Performance optimizations
(setq read-process-output-max (* 1024 1024)) ; 1MB for LSP
(setq process-adaptive-read-buffering nil)

;; Font configuration (optional - adjust to your preference)
(when (member "JetBrains Mono" (font-family-list))
  (set-face-attribute 'default nil :font "JetBrains Mono-12"))

;; Restore garbage collection settings after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216) ; 16MB
            (setq gc-cons-percentage 0.1)))

