;;; early-init.el --- Early initialization configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is loaded before the package system and GUI is initialized.
;; It's the ideal place for package archives setup and early optimizations.

;;; Code:

;; XDG Base Directory support
(defvar my/xdg-config-home (or (getenv "XDG_CONFIG_HOME") (expand-file-name "~/.config")))
(defvar my/xdg-data-home (or (getenv "XDG_DATA_HOME") (expand-file-name "~/.local/share")))
(defvar my/xdg-cache-home (or (getenv "XDG_CACHE_HOME") (expand-file-name "~/.cache")))
(defvar my/xdg-state-home (or (getenv "XDG_STATE_HOME") (expand-file-name "~/.local/state")))

;; Redirect config to XDG_CONFIG_HOME/emacs
(setq user-emacs-directory (expand-file-name "emacs/" my/xdg-config-home))

;; Ensure XDG directories exist
(dolist (dir (list user-emacs-directory
                   (expand-file-name "emacs/" my/xdg-data-home)
                   (expand-file-name "emacs/" my/xdg-cache-home)
                   (expand-file-name "emacs/" my/xdg-state-home)))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; -----------------------------------------------------------------------------
;; Basic UI Framework and Style
;; -----------------------------------------------------------------------------

(defun my/apply-font-config (&optional frame)
  "Apply the GUI font stack to FRAME or the current frame."
  (when (display-graphic-p frame)
    (let ((frame (or frame (selected-frame))))
      (set-face-attribute 'default frame :family "JetBrains Mono" :height 120)
      (dolist (range '((#xE000 . #xF8FF)
                       (#xF0000 . #xFFFFD)
                       (#x100000 . #x10FFFD)))
        (set-fontset-font t range
                          (font-spec :family "Symbols Nerd Font Mono")
                          frame)))))

(add-to-list 'default-frame-alist '(font . "JetBrains Mono-12"))
(add-hook 'window-setup-hook #'my/apply-font-config)
(add-hook 'after-make-frame-functions #'my/apply-font-config)

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

;; Redirect package and native compilation cache to XDG
(setq package-user-dir (expand-file-name "emacs/elpa/" my/xdg-data-home))
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache (expand-file-name "emacs/eln-cache/" my/xdg-cache-home)))

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
