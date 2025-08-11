;;; modules/tools/org/config.el -*- lexical-binding: t; -*-

;;; Simple and Standard Org Mode Configuration

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  
  :custom
  ;; Core settings
  (org-directory "~/org/")
  (org-default-notes-file (concat org-directory "tmp.org"))
  
  ;; Basic appearance
  (org-startup-folded 'content)
  (org-hide-emphasis-markers nil)
  (org-pretty-entities t)
  
  ;; TODO and logging
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "DOING(g)" "WAITING(w)" "|" "DONE(d!)" "CANCELLED(c!)")))
  (org-log-done 'time)
  
  ;; Agenda
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  
  ;; Refiling
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-targets '((nil :maxlevel . 3)
                        (org-agenda-files :maxlevel . 3)))
  
  ;; Archive
  (org-archive-location "archive/%s::datetree/")
  
  ;; Simple capture templates
  (org-capture-templates
   '(("t" "Quick Task" entry (file (lambda () (expand-file-name (format-time-string "tasks/%Y/%Y-%m.org") org-directory)))
      "* TODO %?\n  %U")
     ("T" "Task" entry (file (lambda () (expand-file-name (format-time-string "tasks/%Y/%Y-%m.org") org-directory)))
      "* TODO %?\n  SCHEDULED: %t\n  %U\n  %a\n  %i"
      :empty-lines 1)
     ("j" "Journal" entry (file (lambda () (expand-file-name (format-time-string "journals/%Y/%Y-%m.org") org-directory)))
      "* %?\n  %U\n  %i\n  %a")))
  
  :config
  ;; Ensure org directory exists
  (unless (file-directory-p org-directory)
    (make-directory org-directory t))
  ;; Ensure journals and tasks directories exist
  (unless (file-directory-p (concat org-directory "journals/"))
    (make-directory (concat org-directory "journals/") t))
  (unless (file-directory-p (concat org-directory "tasks/"))
    (make-directory (concat org-directory "tasks/") t))

  ;; Set agenda files to all org files in tasks directory
  (let ((tasks-dir (expand-file-name "tasks/" org-directory)))
    (when (file-directory-p tasks-dir)
      (setq org-agenda-files (directory-files-recursively tasks-dir "\\.org$"))))
  
  ;; Ensure agenda files are set when agenda is accessed
  (add-hook 'org-agenda-mode-hook 
            (lambda ()
              (unless org-agenda-files
                (let ((tasks-dir (expand-file-name "tasks/" org-directory)))
                  (when (file-directory-p tasks-dir)
                    (setq org-agenda-files (directory-files-recursively tasks-dir "\\.org$")))))))
  
  ;; Enable babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t))))

;; Completely disable dabbrev in org and org-roam modes
(defun disable-dabbrev-in-org ()
  "Completely disable dabbrev completion in org-related modes."
  ;; Remove all dabbrev-related completion functions
  (setq-local completion-at-point-functions
              (list #'org-pcomplete-completion-at-point
                    #'comint-filename-completion))
  
  ;; Disable dabbrev key bindings
  (local-set-key (kbd "M-/") nil)
  (local-set-key (kbd "C-M-/") nil)
  
  ;; Disable hippie-expand if it's enabled
  (when (fboundp 'hippie-expand)
    (local-set-key (kbd "M-/") nil))
  
  ;; Remove dabbrev from completion functions completely
  (setq-local completion-at-point-functions
              (delq #'dabbrev-completion completion-at-point-functions))
  
  ;; Disable auto-completion if it tries to use dabbrev
  (when (bound-and-true-p corfu-mode)
    (setq-local corfu-auto nil)))

;; Apply to org-mode with higher priority
(add-hook 'org-mode-hook #'disable-dabbrev-in-org 90)

;; Also apply when entering any org-related mode
(add-hook 'org-capture-mode-hook #'disable-dabbrev-in-org 90)
(add-hook 'org-agenda-mode-hook #'disable-dabbrev-in-org 90)

;; Modern bullet points (built-in alternative)
(use-package org
  :ensure nil
  :config
  ;; Use built-in org-superstar-like functionality
  (setq org-hide-leading-stars t
        org-odd-levels-only t)
  ;; Custom bullet characters
  (font-lock-add-keywords 'org-mode
    '(("^\\*\\{1\\} " (0 (prog1 () (compose-region (match-beginning 0) (match-end 0) "•"))))
      ("^\\*\\{2\\} " (0 (prog1 () (compose-region (match-beginning 0) (match-end 0) "  ◦"))))
      ("^\\*\\{3\\} " (0 (prog1 () (compose-region (match-beginning 0) (match-end 0) "    ▪")))))))

;; Org-roam configuration
(use-package org-roam
  :ensure t
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :custom
  (org-roam-directory "~/org/roam/")
  (org-roam-db-location "~/org/roam/org-roam.db")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "%<%Y>/%<%s>.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :config
  ;; Ensure roam directory exists
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  ;; Ensure the org-roam database is created
  (org-roam-db-autosync-mode)
  
  ;; Disable dabbrev in all org-roam contexts
  (add-hook 'org-roam-mode-hook #'disable-dabbrev-in-org 90)
  (add-hook 'org-roam-capture-mode-hook #'disable-dabbrev-in-org 90)
  
  ;; Also disable dabbrev when visiting org-roam files
  (defun org-roam-file-setup ()
    "Setup for org-roam files to disable dabbrev."
    (when (and buffer-file-name
               (file-in-directory-p buffer-file-name org-roam-directory))
      (disable-dabbrev-in-org)))
  
  (add-hook 'find-file-hook #'org-roam-file-setup))

;; Global advice to prevent dabbrev functions in org-mode buffers
(defun prevent-dabbrev-in-org (orig-fun &rest args)
  "Prevent dabbrev functions from running in org-mode and org-roam buffers."
  (unless (or (derived-mode-p 'org-mode)
              (and buffer-file-name
                   (string-match-p "\\.org$" buffer-file-name))
              (and (bound-and-true-p org-roam-directory)
                   buffer-file-name
                   (file-in-directory-p buffer-file-name org-roam-directory)))
    (apply orig-fun args)))

;; Apply advice to dabbrev functions
(advice-add 'dabbrev-expand :around #'prevent-dabbrev-in-org)
(advice-add 'dabbrev-completion :around #'prevent-dabbrev-in-org)

(provide 'org-config)
