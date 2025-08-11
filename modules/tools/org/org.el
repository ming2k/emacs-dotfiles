;;; modules/tools/org/org.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Core org-mode configuration including basic settings, appearance, babel, and completion management
;;; Code:

;;; Core Org Configuration
(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("C-c TAB" . org-manual-path-completion)
         ("C-c C-/" . org-manual-path-completion))
  :custom
  ;; Directory structure
  (org-directory "~/org/")
  (org-default-notes-file (concat org-directory "notes.org"))
  
  ;; Display settings
  (org-startup-folded 'content)
  (org-pretty-entities t)
  (org-hide-leading-stars t)
  (org-odd-levels-only t)
  
  ;; TODO workflow
  (org-todo-keywords '((sequence "TODO(t)" "DOING(g)" "|" "DONE(d!)" "CANCELLED(c!)")))
  (org-log-done 'time)
  
  ;; Agenda settings
  (org-agenda-window-setup 'current-window)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  
  ;; Refiling
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-archive-location "archive/%s::datetree/")
  
  ;; Capture templates
  (org-capture-templates
   '(("t" "Task" entry (file+headline org-default-notes-file "Tasks")
      "* TODO %?\n  %U")
     ("j" "Journal" entry (file+datetree org-default-notes-file)
      "* %?\n  %U")))
  
  :config
  ;; Ensure org directory exists
  (unless (file-directory-p org-directory)
    (make-directory org-directory t))
  
  ;; Load babel languages for code execution
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)))
  
  ;; Babel execution settings
  (setq org-confirm-babel-evaluate nil  ; Don't prompt before evaluating code blocks
        org-babel-results-keyword "RESULTS"
        org-babel-noweb-wrap-start "<<"
        org-babel-noweb-wrap-end ">>"
        org-src-fontify-natively t       ; Syntax highlighting in source blocks
        org-src-tab-acts-natively t      ; Tab acts normally in source blocks
        org-edit-src-content-indentation 0) ; No extra indentation in source blocks
  
  ;; Manual file path completion function for org-mode
  (defun org-manual-path-completion ()
    "Manual file path completion for org-mode."
    (interactive)
    (let ((completion-at-point-functions '(comint-filename-completion)))
      (completion-at-point)))
  
  ;; Disable automatic completion and dabbrev in org-mode to prevent interference
  (defun org-disable-completion ()
    "Disable automatic completion and dabbrev in org-mode."
    ;; Disable corfu auto-completion
    (setq-local corfu-auto nil)
    ;; Completely clear completion functions to avoid dabbrev errors
    (setq-local completion-at-point-functions nil))
  
  (add-hook 'org-mode-hook #'org-disable-completion)
  )

(provide 'org-core-config)
;;; modules/tools/org/org.el ends here
