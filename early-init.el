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
(add-to-list 'default-frame-alist '(height . 35))

;; Set all new frames to be created without decorations by default
(add-to-list 'default-frame-alist '(undecorated . t))
;; Set first frame color
(add-to-list 'default-frame-alist '(background-color . "#1e1e1e"))
(add-to-list 'default-frame-alist '(foreground-color . "#d4d4d4"))

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
;; Builtin Pacakge Manager
;; -----------------------------------------------------------------------------

;; Add package archives
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Initialize package system
(package-initialize)

;; Add all module directories to load-path early for require/provide system
(let ((modules-dir (expand-file-name "modules" user-emacs-directory)))
  (when (file-directory-p modules-dir)
    ;; Add the main modules directory
    (add-to-list 'load-path modules-dir)
    ;; Add category directories (like modules/lang for lang-config.el)
    (dolist (category (directory-files modules-dir t "^[^.]"))
      (when (file-directory-p category)
        (add-to-list 'load-path category)
        ;; Add all individual module subdirectories
        (dolist (module (directory-files category t "^[^.]"))
          (when (file-directory-p module)
            (add-to-list 'load-path module)))))))

;; -----------------------------------------------------------------------------
;; Perfermance
;; -----------------------------------------------------------------------------

;; Performance optimizations
(setq read-process-output-max (* 1024 1024)) ; 1MB for LSP
(setq process-adaptive-read-buffering nil)

;; Increase garbage collection threshold during startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Restore garbage collection settings after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216) ; 16MB
            (setq gc-cons-percentage 0.1)))

(provide 'early-init)
;;; early-init.el ends here
