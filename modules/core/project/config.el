;;; modules/core/project/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Project management and navigation configuration
;;; Code:

;; Enhanced project search with better matching
(use-package project
  :ensure nil
  :config
  ;; Use built-in commands for project file finding
  (setq project-switch-commands
        '((project-find-file "Find file" ?f)
          (project-find-regexp "Find regexp" ?g)
          (magit-project-status "Magit" ?m)))
  
  ;; Better project file completion
  (defun project-find-file-fuzzy ()
    "Find file in project with fuzzy matching."
    (interactive)
    (let ((completion-styles '(orderless)))
      (project-find-file)))
  
  ;; Project-aware search commands using built-ins
  (defun project-search-regexp ()
    "Search in project using built-in regexp search."
    (interactive)
    (if-let ((project (project-current)))
        (call-interactively #'project-find-regexp)
      (call-interactively #'grep-find)))
  
  (defun project-find-files ()
    "Find files in project using built-in find."
    (interactive)
    (if-let ((project (project-current)))
        (project-find-file)
      (find-file)))
  
  :bind (("C-x p f" . project-find-file-fuzzy)
         ("C-x p g" . project-search-regexp)
         ("C-x p F" . project-find-files)))

;;; modules/core/project/config.el ends here