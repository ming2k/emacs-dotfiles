;;; init-session.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Session management with desktop, savehist, saveplace and recentf
;; Optimized for session persistence and restoration

;;; Code:

;; Desktop save mode for session persistence
(use-package desktop
  :ensure nil
  :init
  (desktop-save-mode 1)
  :config
  ;; Performance optimizations for faster startup
  (setq desktop-restore-frames t
        desktop-restore-in-current-display nil
        desktop-restore-reuses-frames t     ; Reuse existing frames
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

;; Recent files tracking
(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-saved-items 50
        recentf-max-menu-items 15
        recentf-exclude '("COMMIT_EDITMSG\\'"
                         ".*-autoloads\\.el\\'"
                         "[/\\\\]\\.elpa/")))

;; Save minibuffer history
(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1)
  :config
  (setq savehist-length 25
        savehist-save-minibuffer-history t
        savehist-additional-variables
        '(mark-ring
          global-mark-ring
          search-ring
          regexp-search-ring
          extended-command-history)))

;; Save point position in files
(use-package saveplace
  :ensure nil
  :init
  (save-place-mode 1)
  :config
  (setq save-place-file (expand-file-name "places" user-emacs-directory)))

(provide 'init-session)
;;; init-session.el ends here
