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
  (org-agenda-files (list (concat org-directory "journals/")
                          (concat org-directory "tasks/")))
  
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
  
  ;; Enable babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t))))

;; Disable dabbrev in org mode
(add-hook 'org-mode-hook
          (lambda ()
            ;; Remove dabbrev from completion functions
            (setq-local completion-at-point-functions
                        (remove #'cape-dabbrev completion-at-point-functions))
            ;; Disable dabbrev key binding
            (define-key org-mode-map (kbd "M-/") nil)))

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

;; Template expansion
(use-package org-tempo
  :ensure nil
  :after org
  :config
  ;; Add structure templates after org-tempo is loaded
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

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
  (org-roam-db-autosync-mode))

(provide 'org-config)
