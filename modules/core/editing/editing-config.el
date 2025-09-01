;;; modules/core/editing/editing-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Core editing features including desktop session management and fill-column settings
;;; Code:

;; Load fill-column configuration
(load (expand-file-name "fill-column.el" (file-name-directory load-file-name)))

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

(provide 'editing-config)

;;; modules/core/editing/editing-config.el ends here