;;; config/core/editing.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Core editing features including desktop session management and fill-column settings
;;; Code:

;; Set default fill column
(setq-default fill-column 80)

;; Fill column indicator and auto-fill mode are NOT enabled globally
;; Let modes decide for themselves based on their needs

;; Custom fill column settings for specific modes
(defun set-fill-column-for-mode (mode column)
  "Set fill-column for a specific MODE to COLUMN value."
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            `(lambda () (setq-local fill-column ,column))))

;; Mode-specific fill column settings
(set-fill-column-for-mode 'emacs-lisp-mode 80)
(set-fill-column-for-mode 'lisp-mode 80)
(set-fill-column-for-mode 'python-mode 88)  ; Black formatter standard
(set-fill-column-for-mode 'rust-mode 100)   ; Rust standard
(set-fill-column-for-mode 'go-mode 100)     ; Go standard

;; Individual modes can enable fill-column-indicator and auto-fill-mode as needed

(use-package desktop
  :ensure nil
  :init
  ;; Enable desktop mode at startup to autoload desktop cache
  (desktop-save-mode 1)
  :config
  ;; Basic desktop configuration
  (setq desktop-save t
        desktop-load-locked-desktop t
        desktop-restore-frames t
        desktop-restore-in-current-display t
        desktop-restore-reuses-frames t
        desktop-auto-save-timeout 300)  ; Auto-save every 5 minutes
  
  ;; Don't restore certain buffers that cause issues
  (setq desktop-buffers-not-to-save
        "\\(^nn\\.\\|^tags\\|^TAGS\\|^\\*.*\\*\\)")
  
  ;; Automatically load desktop at startup
  (desktop-read))

(provide 'editing)
;;; config/core/editing.el ends here