;;; config/tools/org-roam.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Org-roam configuration for Zettelkasten-style note-taking and knowledge management
;; Includes aggressive completion disabling to prevent interference with org workflows
;;; Code:
  
;;; Org-Roam Package Configuration
(use-package org-roam
  :ensure t
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Optional additional bindings (uncomment as needed):
         ;; ("C-c n g" . org-roam-graph)
         ;; ("C-c n r" . org-roam-node-random)
         ;; ("C-c n t" . org-roam-tag-add)
         ;; ("C-c n a" . org-roam-alias-add)
         ;; ("C-c n l" . org-roam-buffer-toggle)
         )
  :config
  ;; Set org-roam directory and database location first
  (setq org-roam-directory (expand-file-name "~/org/roam/"))
  (setq org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  (setq org-roam-completion-everywhere nil)
  (setq org-roam-completion-ignore-case nil)
  (setq org-roam-completion-system nil)
  
  ;; Ensure the directory exists
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  
  ;; Simple default capture template
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y/%m/%d>/%<%Y%m%d%H%M%S>.org"
                              "#+title: ${title}\n#+created: %U\n")
           :unnarrowed t)))
  
  ;; Enable automatic database sync (only if function exists)
  (when (fboundp 'org-roam-db-autosync-mode)
    (org-roam-db-autosync-mode))
  
  ;; Define date directory function
  (defun org-roam-ensure-date-directory (&rest _)
    "Ensure the current date directory structure exists in org-roam directory."
    (when org-roam-directory
      (let* ((today (format-time-string "%Y/%m/%d"))
             (date-dir (expand-file-name today org-roam-directory)))
        (unless (file-directory-p date-dir)
          (make-directory date-dir t)))))
  
  ;; Hook to create date directories automatically
  (advice-add 'org-roam-capture- :before #'org-roam-ensure-date-directory))
  

(provide 'org-roam-config)
;;; config/tools/org-roam.el ends here
