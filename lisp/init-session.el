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
        desktop-restore-eager 5            ; Only restore first x buffers eagerly
        desktop-path (list (expand-file-name "emacs" my/xdg-state-home))
        desktop-dirname (expand-file-name "emacs" my/xdg-state-home))

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
          vc-log-edit-mode))

  ;; NOTE: Do NOT save org-roam histories via desktop.
  ;; They are already handled by savehist-mode. Saving them in both
  ;; places causes races on exit where desktop overwrites the
  ;; richer savehist state with its stale copy, leaving only one
  ;; entry in the history file.
  ;; (add-to-list 'desktop-globals-to-save 'org-roam-node-history)
  ;; (add-to-list 'desktop-globals-to-save 'org-roam-ref-history)
  )

;; Recent files tracking
(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-saved-items 50
        recentf-max-menu-items 15
        recentf-save-file (expand-file-name "emacs/recentf" my/xdg-state-home)
        recentf-exclude '("COMMIT_EDITMSG\\'"
                         ".*-autoloads\\.el\\'"
                         "[/\\\\]\\.elpa/")))

;; Save minibuffer history
(use-package savehist
  :ensure nil
  :init
  ;; Must set before enabling mode so savehist loads these variables on startup.
  (setq savehist-file (expand-file-name "emacs/history" my/xdg-state-home)
        savehist-additional-variables
        '(mark-ring
          global-mark-ring
          search-ring
          regexp-search-ring
          extended-command-history
          org-roam-node-history
          org-roam-ref-history))
  (savehist-mode 1)
  :config
  (setq savehist-length 100
        savehist-save-minibuffer-history t))

;; Save point position in files
(use-package saveplace
  :ensure nil
  :init
  (save-place-mode 1)
  :config
  (setq save-place-file (expand-file-name "emacs/places" my/xdg-state-home)))

(provide 'init-session)
;;; init-session.el ends here
