;;; config/core/desktop-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Session management with desktop, savehist, and recentf integration
;; Optimized for fast startup with limited buffer restoration
;;; Code:

;; Desktop save mode for session persistence
(use-package desktop
  :ensure nil
  :init
  (desktop-save-mode 1)
  :config
  ;; Performance optimizations for faster startup
  (setq desktop-restore-frames nil          ; Don't restore frame configuration
        desktop-restore-in-current-display t ; Use current display
        desktop-restore-reuses-frames t     ; Reuse existing frames
        desktop-lazy-verbose nil            ; Reduce verbosity
        desktop-lazy-idle-delay 1           ; Delay before lazy loading
        desktop-auto-save-timeout 30        ; Auto-save every 30 seconds
        desktop-restore-eager 5)           ; Only restore first x buffers eagerly
  
  ;; Limit number of buffers to restore for faster startup
  (setq desktop-buffers-not-to-save
        (concat "\\("
                "^\\*.*\\*$\\|"              ; Don't save *scratch*, *Messages*, etc.
                "COMMIT_EDITMSG\\|"          ; Git commit messages
                "\\)"))
  
  ;; Files to exclude from desktop restoration
  (setq desktop-files-not-to-save
        "\\(^/[^/:]*:\\|(ftp)$\\|^/tmp/\\|\\.gpg$\\)")
  
  ;; Modes to exclude from desktop restoration
  (setq desktop-modes-not-to-save
        '(tags-table-mode
          log-edit-mode
          magit-log-edit-mode
          vc-log-edit-mode)))

;; Recent files tracking (disabled for desktop mode)
;; (use-package recentf
;;   :ensure nil)

;; Save minibuffer history (disabled for desktop mode)
;; (use-package savehist
;;   :ensure nil)

;; Save point position in files (disabled for desktop mode)
;; (use-package saveplace
;;   :ensure nil)

(provide 'desktop-config)
;;; config/core/session.el ends here

