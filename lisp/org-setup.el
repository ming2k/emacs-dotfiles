;;; org-setup.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Complete Org-mode configuration including core org and org-roam for GTD workflow
;; and Zettelkasten-style note-taking
;;; Code:

;;; Core Org Configuration
(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :hook
  (org-mode . visual-line-mode)
  :custom
  ;; Directory structure
  (org-directory "~/org/")
  (org-default-notes-file "~/org/inbox.org")

  ;; Modify the behavior of creating date/datetime to include the time zone
  ;;(org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M %Z>"))
  
  ;; Display settings
  (org-startup-folded nil)
  
  ;; Remember fold status
  (org-cycle-global-at-bob t)
  (org-startup-with-inline-images t)
  
  ;; TODO Keywords with logging
  ;; ! = log timestamp, @ = prompt for note
  ;; Format: STATE(key!/@@) where first is entering, second is leaving
  (org-todo-keywords
   '((sequence "TODO(t!)" "IN-PROGRESS(i!)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))

  ;; Logging configuration
  ;; Add CLOSED: [timestamp] property when marking DONE (also logs to LOGBOOK via d!)
  (org-log-done 'time)
  ;; Alternative: use 'note to prompt for completion note
  ;; (org-log-done 'note)

  ;; Log state changes into LOGBOOK drawer
  (org-log-into-drawer t)

  ;; Log state changes for repeating tasks
  (org-log-repeat 'time)

  ;; Agenda settings
  (org-agenda-files 
   '("~/org/inbox.org"
     "~/org/next.org"
     "~/org/waiting.org"))
  
  ;; Refiling
  (org-refile-targets
   '(("~/org/next.org" :maxlevel . 1)
     ("~/org/waiting.org" :maxlevel . 1)
     (nil :maxlevel . 2)))
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  
  ;; Capture templates
  (org-capture-templates
   '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Inbox")
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
     ("i" "Idea/Thought" entry (file+headline "~/org/inbox.org" "Inbox")
      "* %?\n  %U")
     ("r" "Remember" entry (file+headline "~/org/inbox.org" "Inbox")
      "* %?\n  %U\n  %a")))

  :config
  ;; Set font for org tables
  (set-face-attribute 'org-table nil :font "Sarasa Mono SC-13")

  ;; Enable truncate-lines in org tables
  (defun my/org-table-toggle-truncate-lines ()
    "Toggle truncate-lines based on whether point is in an org table."
    (when (derived-mode-p 'org-mode)
      (setq truncate-lines (org-at-table-p))))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'post-command-hook #'my/org-table-toggle-truncate-lines nil t)))

  ;; Archive configuration - quarterly archiving
  (defun my/quarterly-archive-location ()
    "Generate quarterly archive location string.
    %s in org-archive-location is replaced by org-mode with the source filename (without .org extension)."
    (let* ((year (format-time-string "%Y"))
           (month (string-to-number (format-time-string "%m")))
           (quarter (cond
                     ((<= month 3) "Q1")
                     ((<= month 6) "Q2")
                     ((<= month 9) "Q3")
                     (t "Q4"))))
      ;; %%s becomes %s after format processing, which org-mode replaces with filename
      (format "~/org/archive/%%s-%s%s.org::" year quarter)))

  ;; Set archive location
  (setq org-archive-location (my/quarterly-archive-location))
  
  ;; Update archive location daily to handle quarter transitions  
  (run-with-timer 0 86400 
                  (lambda () 
                    (setq org-archive-location (my/quarterly-archive-location))))
   
  ;; Ensure org directory exists
  (unless (file-directory-p org-directory)
    (make-directory org-directory t))
  
  ;; Ensure archive directory exists
  (let ((archive-dir (expand-file-name "archive" org-directory)))
    (unless (file-directory-p archive-dir)
      (make-directory archive-dir t)))
  
  ;; Create GTD style structure
  (let ((org-files '(("inbox.org" . "* Inbox\n")
                     ("next.org" . "* Next\n")
                     ("waiting.org" . "* Waiting\n"))))
    (dolist (file-info org-files)
      (let ((filepath (expand-file-name (car file-info) org-directory))
            (content (cdr file-info)))
        (unless (file-exists-p filepath)
          (with-temp-file filepath
            (insert content))
          (message "Created org file: %s" filepath))))))

;;; Org-Roam Configuration  
(use-package org-roam
  :ensure t
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n c" . org-roam-capture)
         ("C-c n i" . org-roam-node-insert))
  :config
  ;; Set org-roam directory and database location first
  (setq org-roam-directory (expand-file-name "~/org-roam/"))
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
           ;; :target (file+head "%<%Y/%m/%d>/%<%Y%m%d%H%M%S>.org"
           :target (file+head "%<%Y>/%<%Y%m%dT%H%M%S%z>.org" ; Following ISO 8601 format "20250903T143052+0800"
                              "#+title: ${title}\n#+created: %U\n")
           :unnarrowed t)))
  
  ;; Enable automatic database sync (only if function exists)
  (when (fboundp 'org-roam-db-autosync-mode)
    (org-roam-db-autosync-mode))
  
  ;; Define date directory function
  (defun org-roam-ensure-date-directory (&rest _)
    "Ensure the current date directory structure exists in org-roam directory."
    (when org-roam-directory
      (let* ((today (format-time-string "%Y"))
             (date-dir (expand-file-name today org-roam-directory)))
        (unless (file-directory-p date-dir)
          (make-directory date-dir t)))))
  
  ;; Hook to create date directories automatically
  (advice-add 'org-roam-capture- :before #'org-roam-ensure-date-directory))
;;; Org-Babel Configuration
(use-package ob
  :ensure nil
  :after org
  :config
  ;; Load basic languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t))))


(provide 'org-setup)
;;; org-setup.el ends here
