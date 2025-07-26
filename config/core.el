;; config/core.el - Core settings

;;; Commentary:
;; Core Emacs settings and variables

;;; Code:

;; Enhanced cursor position saving (configured below)

;; UTF-8 everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Better defaults
(setq-default
 ;; Indentation
 indent-tabs-mode nil
 tab-width 4
 fill-column 80
 
 ;; Editing
 require-final-newline t
 sentence-end-double-space nil)


;; Auto-save and backup settings
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 3
      kept-old-versions 2
      version-control t)

;; Recentf
(use-package recentf
  :init
  :config
  (setq recentf-max-saved-items 100)
  (recentf-mode 1))

;; Session and cursor position management
(use-package saveplace
  :ensure nil
  :config
  (setq save-place-limit 1000
        save-place-forget-unreadable-files t))

;; Desktop session support (optional - uncomment to enable)
(use-package desktop
  :config
  (desktop-save-mode 1)
  :custom
  (desktop-restore-eager 8)
  (desktop-load-locked-desktop t)
  (desktop-auto-save-timeout 30))

;; Enhanced cursor position features
(defun save-cursor-position-advice ()
  "Enhanced cursor position saving with additional context."
  (when buffer-file-name
    (let ((position (point))
          (line (line-number-at-pos))
          (column (current-column)))
      (message "Saving position: line %d, column %d" line column))))

(advice-add 'save-place-to-alist :after #'save-cursor-position-advice)

;; Auto-save cursor position more frequently
(add-hook 'focus-out-hook #'save-place-kill-emacs-hook)
(add-hook 'auto-save-hook #'save-place-kill-emacs-hook)

;; Restore window configuration and cursor position
(defun restore-cursor-position-enhanced ()
  "Enhanced cursor position restoration with visual feedback."
  (when (and buffer-file-name
             (file-exists-p buffer-file-name))
    (let ((saved-place (assoc buffer-file-name save-place-alist)))
      (when saved-place
        (goto-char (cdr saved-place))
        (when (> (line-number-at-pos) (window-height))
          (recenter))
        (message "Restored to line %d, column %d" 
                 (line-number-at-pos) (current-column))))))

(add-hook 'find-file-hook #'restore-cursor-position-enhanced)

;; Quick commands for cursor position management
(defun save-cursor-position-manually ()
  "Manually save current cursor position."
  (interactive)
  (save-place-to-alist)
  (message "Cursor position saved for %s" (buffer-name)))

(defun show-cursor-position-info ()
  "Show detailed cursor position information."
  (interactive)
  (let ((line (line-number-at-pos))
        (column (current-column))
        (char (char-after))
        (total-lines (line-number-at-pos (point-max))))
    (message "Line %d/%d, Column %d, Char: %s (0x%02X)" 
             line total-lines column 
             (if char (char-to-string char) "EOF")
             (or char 0))))

;; Global keybindings for cursor position features
(global-set-key (kbd "C-c p s") 'save-cursor-position-manually)
(global-set-key (kbd "C-c p i") 'show-cursor-position-info)

;; Enable useful features
(electric-pair-mode 1)
(show-paren-mode 1)
(global-auto-revert-mode 1)
(delete-selection-mode 1)

(provide 'core)
;;; core.el ends here
