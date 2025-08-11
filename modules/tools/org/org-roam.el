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
  (org-roam-completion-everywhere nil)
  (org-roam-completion-ignore-case nil)
  (org-roam-completion-system nil)
  
  ;; Optional: Customize org-roam capture templates
  ;; Uncomment and modify as needed:
  ;; (org-roam-capture-templates
  ;;  '(("d" "default" plain "%?"
  ;;     :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
  ;;                        "#+title: ${title}\n")
  ;;     :unnarrowed t)
  ;;    ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
  ;;     :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
  ;;                        "#+title: ${title}\n#+filetags: Project\n")
  ;;     :unnarrowed t)))
  
  :config
  ;; Ensure roam directory exists
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  
  ;; Enable automatic database sync
  (org-roam-db-autosync-mode)
  
  ;; Disable automatic completion and dabbrev in org-roam files
  (defun org-roam-disable-completion ()
    "Disable automatic completion and dabbrev in org-roam buffers."
    ;; Disable corfu auto-completion
    (setq-local corfu-auto nil)
    ;; Completely clear completion functions to avoid dabbrev errors
    (setq-local completion-at-point-functions nil))
  
  ;; Apply completion disabling to org-roam files
  (add-hook 'org-roam-find-file-hook #'org-roam-disable-completion)
)  

(provide 'org-roam-config)
;;; modules/tools/org/org-roam.el ends here
