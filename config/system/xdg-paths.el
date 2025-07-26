;;; config/system/xdg-paths.el --- Essential XDG Base Directory paths -*- lexical-binding: t; -*-
;;; Commentary:
;; Essential XDG Base Directory Specification support for early initialization
;; https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
;;; Code:

;; XDG Base Directories
(defvar xdg-config-home
  (or (getenv "XDG_CONFIG_HOME")
      (expand-file-name "~/.config"))
  "XDG config directory")

(defvar xdg-data-home
  (or (getenv "XDG_DATA_HOME")
      (expand-file-name "~/.local/share"))
  "XDG data directory")

(defvar xdg-cache-home
  (or (getenv "XDG_CACHE_HOME")
      (expand-file-name "~/.cache"))
  "XDG cache directory")

(defvar xdg-state-home
  (or (getenv "XDG_STATE_HOME")
      (expand-file-name "~/.local/state"))
  "XDG state directory")

;; Helper functions
(defun ensure-directory (dir)
  "Ensure directory exists, create if it doesn't."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun xdg-emacs-path (base-dir subdir)
  "Create emacs subdirectory path under XDG base directory."
  (let ((path (expand-file-name (concat "emacs/" subdir) base-dir)))
    (ensure-directory (file-name-directory path))
    path))

;; Package directory (needed early for package system)
(setq package-user-dir (xdg-emacs-path xdg-data-home "elpa/"))

;; Native compilation cache (critical for performance)
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache (xdg-emacs-path xdg-cache-home "eln-cache/")))

(when (bound-and-true-p native-comp-eln-load-path)
  (setq comp-eln-directory (xdg-emacs-path xdg-cache-home "eln-cache/")))

;; Essential backup/autosave (prevents file clutter)
(setq backup-directory-alist `(("." . ,(xdg-emacs-path xdg-state-home "backups/")))
      auto-save-list-file-prefix (xdg-emacs-path xdg-cache-home "auto-save-list/.saves-")
      auto-save-file-name-transforms `((".*" ,(xdg-emacs-path xdg-cache-home "auto-save/") t)))

(provide 'xdg-paths)
;;; xdg-paths.el ends here
