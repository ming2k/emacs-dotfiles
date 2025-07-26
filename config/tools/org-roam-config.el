;;; org-roam-config.el --- Org-roam configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Simple, working org-roam configuration
;;; Code:

;; Create org-roam directory
(defvar my-org-roam-dir (expand-file-name "~/org/"))
(unless (file-directory-p my-org-roam-dir)
  (make-directory my-org-roam-dir t))

;; Simple org-roam setup that WORKS
(with-eval-after-load 'org
  (when (require 'org-roam nil t)
    ;; Set org-roam directory and database location
    (setq org-roam-directory my-org-roam-dir)
    (setq org-roam-db-location (expand-file-name "org-roam.db" my-org-roam-dir))
    
    ;; Configure capture templates with year/month organization
    (setq org-roam-capture-templates
          '(("d" "default" plain "%?"
             :target (file+head "%<%Y/%m>/%<%Y%m%d%H%M%S>.org"
                                "#+title: ${title}\n#+date: %U\n")
             :unnarrowed t)))
    
    ;; Start the database
    (org-roam-db-autosync-mode 1)
    
    ;; Set keybindings
    (global-set-key (kbd "C-c n f") 'org-roam-node-find)
    (global-set-key (kbd "C-c n i") 'org-roam-node-insert)
    (global-set-key (kbd "C-c n c") 'org-roam-capture)
    (global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
    (global-set-key (kbd "C-c n g") 'org-roam-graph)
    (global-set-key (kbd "C-c n j") 'org-roam-dailies-capture-today)
    
    (message "org-roam configured with C-c n keybindings")))

;; Alternative: use-package approach
(use-package org-roam
  :ensure t
  :defer t
  :init
  (setq org-roam-directory my-org-roam-dir)
  (setq org-roam-db-location (expand-file-name "org-roam.db" my-org-roam-dir))
  :config
  ;; Organize files by year/month
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y/%m>/%<%Y%m%d%H%M%S>.org"
                              "#+title: ${title}\n#+date: %U\n")
           :unnarrowed t)))
  
  ;; Set up dailies in year/month structure  
  (setq org-roam-dailies-directory "daily/"
        org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :target (file+head "%<%Y/%m>/%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+date: %U\n"))))
  
  (org-roam-db-autosync-mode 1)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n g" . org-roam-graph)
         ("C-c n j" . org-roam-dailies-capture-today)))

(provide 'org-roam-config)
;;; org-roam-config.el ends here