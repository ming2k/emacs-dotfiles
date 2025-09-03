;;; config/tools/org-config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Core org-mode configuration to complete simplified GTD workflow.

;;; Code:

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
  (org-default-notes-file "~/org/inbox.org")
  
  ;; Display settings
  (org-startup-folded 'content)
  (org-pretty-entities t)
  (org-hide-leading-stars t)
  (org-odd-levels-only t)
  
  ;; TODO Keywords, GTD style
  (org-todo-keywords
   '((sequence "INBOX(i)" "ACTIVE(a)" "|" "DONE(d)" "CANCELLED(c)")))

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
      "* TODO %?\n  %U")
     ("i" "Idea/Thought" entry (file+headline "~/org/inbox.org" "Inbox")
      "* %?\n  %U")
     ("r" "Remember" entry (file+headline "~/org/inbox.org" "Inbox")
      "* %?\n  %U\n  %a")))
  
  :config
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
  (let ((org-files '(("inbox.org" . "#+TITLE: Inbox\n\n* Inbox\n")
                     ("next.org" . "#+TITLE: Next\n\n* Next\n")
                     ("waiting.org" . "#+TITLE: Waiting\n\n* Waiting\n"))))
    (dolist (file-info org-files)
      (let ((filepath (expand-file-name (car file-info) org-directory))
            (content (cdr file-info)))
        (unless (file-exists-p filepath)
          (with-temp-file filepath
            (insert content))
          (message "Created org file: %s" filepath))))))

(provide 'org-config)
;;; config/tools/org-config.el ends here
