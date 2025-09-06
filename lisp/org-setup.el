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
  :custom
  ;; Directory structure
  (org-directory "~/org/")
  (org-default-notes-file "~/org/inbox.org")
  
  ;; Display settings
  (org-startup-folded nil)
  
  ;; Remember fold status
  (org-cycle-global-at-bob t)
  (org-startup-with-inline-images t)
  
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

;;; Org-Roam Configuration  
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
           :target (file+head "<%Y>/%<%Y%m%dT%H%M%S%z>.org" ; Following ISO 8601 format "20250903T143052+0800"
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

;;; Smart Tab Configuration for Org Mode
(with-eval-after-load 'org
  (defun org-smart-tab ()
    "Smart TAB function for org-mode.
    Priority: 1) corfu completion (includes yasnippet via cape) 2) org-cycle"
    (interactive)
    (cond
     ;; First priority: corfu completion (includes yasnippet via cape-yasnippet)
     ((and (bound-and-true-p corfu-mode)
           (let ((completion-result (completion-at-point)))
             (and completion-result
                  (not (eq (car completion-result) (cadr completion-result))))))
      (completion-at-point))
     ;; Second priority: org-cycle (folding, indenting, etc.)
     (t (org-cycle))))
  
  ;; Bind smart tab function to TAB in org-mode
  (define-key org-mode-map (kbd "TAB") 'org-smart-tab)
  (define-key org-mode-map (kbd "<tab>") 'org-smart-tab))

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
