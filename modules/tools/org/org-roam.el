;;; modules/tools/org/org-roam.el -*- lexical-binding: t; -*-
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
  :custom
  (org-roam-directory "~/org/roam/")
  (org-roam-db-location "~/org/roam/org-roam.db")
  (org-roam-completion-everywhere nil)
  (org-roam-completion-ignore-case nil)
  (org-roam-completion-system nil)
  
  ;; Simple default capture template
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y/%m/%d>/%<%Y%m%d%H%M%S>.org"
                         "#+title: ${title}\n#+created: %U\n")
      :unnarrowed t)))
  
  :config
  ;; Ensure roam directory exists
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  
  ;; Hook to create date directories automatically
  (advice-add 'org-roam-capture- :before #'org-roam-ensure-date-directory)
  
  ;; Enable automatic database sync
  (org-roam-db-autosync-mode))

;; Function to ensure date directories exist before capture
(defun org-roam-ensure-date-directory (&rest _)
  "Ensure the current date directory structure exists in org-roam directory."
  (when org-roam-directory
    (let* ((today (format-time-string "%Y/%m/%d"))
           (date-dir (expand-file-name today org-roam-directory)))
      (unless (file-directory-p date-dir)
        (make-directory date-dir t)))))  

(provide 'org-roam-config)
;;; modules/tools/org/org-roam.el ends here
