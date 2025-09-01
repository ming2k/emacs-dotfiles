;;; config/tools/org.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Core org-mode configuration including basic settings, appearance, babel, and completion management
;;; Code:

(defun org-manual-path-completion ()
  "Manual file path completion for org-mode."
  (interactive)
  (let ((completion-at-point-functions '(comint-filename-completion)))
    (completion-at-point)))

;; Disable automatic completion but preserve org-mode built-in completion
(defun org-disable-completion ()
  "Disable automatic completion but preserve org-mode built-in functions."
  ;; Disable corfu auto-completion
  (setq-local corfu-auto nil)
  ;; Keep org-mode's built-in completion functions for templates and structure
  (setq-local completion-at-point-functions
              '(org-completion-at-point pcomplete-completions-at-point))
  ;; Ensure structure templates work with TAB
  (setq-local tab-always-indent 'complete))

;; Setup function to be called when org is loaded
(defun setup-org-completion ()
  "Setup org-mode completion behavior."
  (add-hook 'org-mode-hook #'org-disable-completion)
  ;; Ensure structure templates are enabled
  (require 'org-tempo nil t))

;; Set org directory first
(defvar org-directory "~/org/")

;;; Core Org Configuration
(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :custom
  ;; Directory structure
  (org-directory "~/org/")
  (org-default-notes-file "~/org/notes.org")
  
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
  
  ;; Setup completion behavior
  (setup-org-completion)
  
  ;; Setup org-mode keybindings
  (define-key org-mode-map (kbd "C-c TAB") 'org-manual-path-completion)
  (define-key org-mode-map (kbd "C-c C-/") 'org-manual-path-completion))

(provide 'org-config)
;;; config/tools/org.el ends here
