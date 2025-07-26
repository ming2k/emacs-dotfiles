;;; config/tools/org-roam.el --- Simplified Org-roam configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimalist org-roam configuration using only built-in features
;; Focus on simplicity and standard org-roam functionality

;;; Code:

;;; Org Roam Configuration
(use-package org-roam
  :ensure t
  :after org
  :demand t
  :custom
  ;; Core settings
  (org-roam-directory (file-truename "~/notes"))
  (org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  (org-roam-completion-everywhere t)
  
  ;; Simple display - just title
  (org-roam-node-display-template "${title}")
  
  ;; Graph settings
  (org-roam-graph-viewer (if (eq system-type 'darwin) "open" "xdg-open"))
  
  :bind (("C-c n f" . org-roam-node-find)      ; Find
         ("C-c n i" . org-roam-node-insert)    ; Insert link
         ("C-c n c" . org-roam-capture)        ; Capture
         ("C-c n l" . org-roam-buffer-toggle)  ; Links
         ("C-c n g" . org-roam-graph)          ; Graph
         ("C-c n d" . org-roam-dailies-goto-today))  ; Daily
  
  :config
  ;; Ensure directory exists
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  
  ;; Initialize database
  (org-roam-db-autosync-mode 1)
  
  ;; Standard capture templates
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :target (file+head "%<%Y%m%d%H%M%S>.org"
                              "#+title: ${title}\n#+date: %U\n\n")
           :unnarrowed t)))
  
  ;; Standard daily notes
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %<%H:%M> %?"
           :target (file+head "%<%Y%m%d%H%M%S>.org"
                              "#+title: %<%Y-%m-%d>\n#+date: %U\n\n")))))

(provide 'org-roam)
;;; org-roam.el ends here
