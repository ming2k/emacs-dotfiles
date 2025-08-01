;;; modules/core/session/config.el -*- lexical-binding: t; -*-
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
;;   :ensure nil
;;   :config
;;   (setq recentf-max-saved-items 50          ; Reduced from 100 for faster startup
;;         recentf-max-menu-items 15           ; Limit menu items
;;         recentf-auto-cleanup 300            ; Clean up every 5 minutes
;;         recentf-exclude '("/tmp/" "/ssh:" "/sudo:" "\\.?#" "~$" "\\.log$"))
;;   
;;   ;; Integration with desktop - only enable if desktop is active
;;   (when desktop-save-mode
;;     (recentf-mode 1)
;;     ;; Add recentf-save-list to desktop globals
;;     (add-to-list 'desktop-globals-to-save 'recentf-list)))

;; Save minibuffer history (disabled for desktop mode)
;; (use-package savehist
;;   :ensure nil
;;   :config
;;   (setq savehist-length 500                 ; Reduced from 1000 for faster startup
;;         savehist-file (expand-file-name "savehist" desktop-dirname)
;;         savehist-additional-variables '(search-ring
;;                                        regexp-search-ring
;;                                        extended-command-history
;;                                        kill-ring))
;;   
;;   ;; Integration with desktop - only enable if desktop is active
;;   (when desktop-save-mode
;;     (savehist-mode 1)))

;; Save point position in files (disabled for desktop mode)
;; (use-package saveplace
;;   :ensure nil
;;   :config
;;   (setq save-place-limit 500                ; Reduced from 1000 for faster startup
;;         save-place-forget-unreadable-files t
;;         save-place-file (expand-file-name "saveplace" desktop-dirname))
;;   
;;   ;; Integration with desktop - only enable if desktop is active
;;   (when desktop-save-mode
;;     (save-place-mode 1)
;;     ;; Add save-place-alist to desktop globals
;;     (add-to-list 'desktop-globals-to-save 'save-place-alist)))

;; Optional: Add hook to clean up desktop on exit
(add-hook 'kill-emacs-hook 'desktop-save-in-desktop-dir)

;;; modules/core/session/config.el ends here
