;;; modules/core/desktop/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Project-aware desktop session management for layout persistence
;;; Code:

(use-package desktop
  :ensure nil
  :config
  ;; Disable global desktop mode - we only use project-specific desktops
  (desktop-save-mode 0)
  
  ;; Configure desktop behavior for project-specific use
  (setq desktop-save t
        desktop-load-locked-desktop t
        desktop-restore-frames t
        desktop-restore-in-current-display t
        desktop-restore-reuses-frames t
        desktop-auto-save-timeout nil)  ;; Disable auto-save timer
  
  ;; Don't restore certain buffers that cause issues
  (setq desktop-buffers-not-to-save
        "\\(^nn\\.\\|^tags\\|^TAGS\\|^\\*.*\\*\\)"))

;; Project-specific desktop management
(defvar project-desktop-cache-dir
  (expand-file-name "project-desktops/" user-emacs-directory)
  "Directory to store project-specific desktop files.")

(defun project-desktop-file-name ()
  "Generate desktop file name for current project."
  (when-let ((project (project-current)))
    (let* ((project-root (project-root project))
           (project-name (file-name-nondirectory 
                         (directory-file-name project-root)))
           (project-hash (secure-hash 'md5 project-root)))
      (expand-file-name 
       (format "%s-%s.desktop" project-name (substring project-hash 0 8))
       project-desktop-cache-dir))))

(defun project-desktop-save ()
  "Save current desktop layout for the current project."
  (interactive)
  (when-let ((desktop-file (project-desktop-file-name)))
    (unless (file-exists-p project-desktop-cache-dir)
      (make-directory project-desktop-cache-dir t))
    (let ((desktop-dirname (file-name-directory desktop-file))
          (desktop-base-file-name (file-name-nondirectory desktop-file)))
      (desktop-save desktop-dirname t)
      (message "Project layout saved: %s" (file-name-nondirectory desktop-file)))))

(defun project-desktop-restore ()
  "Restore desktop layout for the current project."
  (interactive)
  (when-let ((desktop-file (project-desktop-file-name)))
    (when (file-exists-p desktop-file)
      (let ((desktop-dirname (file-name-directory desktop-file))
            (desktop-base-file-name (file-name-nondirectory desktop-file)))
        (desktop-read desktop-dirname)
        (message "Project layout restored: %s" (file-name-nondirectory desktop-file))))))

(defun project-desktop-auto-save ()
  "Automatically save desktop when switching away from project."
  (when (and (project-current) 
             (project-desktop-file-name))
    (project-desktop-save)))

(defun project-desktop-auto-restore ()
  "Automatically restore desktop when opening project."
  (when (and (project-current)
             (project-desktop-file-name))
    (project-desktop-restore)))

;; Hook into project switching
(add-hook 'project-find-file-hook #'project-desktop-auto-restore)
(add-hook 'kill-emacs-hook #'project-desktop-auto-save)

;; Enhanced project switching with layout management
(defun project-switch-with-layout (project-dir)
  "Switch to project and restore its layout."
  (interactive (list (project-prompt-project-dir)))
  (let ((project (project--find-in-directory project-dir)))
    ;; Save current project layout if we're in one
    (when (project-current)
      (project-desktop-auto-save))
    ;; Switch to new project
    (project-switch-project project-dir)
    ;; Restore new project layout
    (project-desktop-auto-restore)))

;; Keybindings
(define-key project-prefix-map (kbd "s") #'project-desktop-save)
(define-key project-prefix-map (kbd "r") #'project-desktop-restore)
(define-key project-prefix-map (kbd "p") #'project-switch-with-layout)

;;; modules/core/desktop/config.el ends here