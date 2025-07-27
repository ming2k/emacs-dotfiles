;;; modules/tools/org/config.el -*- lexical-binding: t; -*-

(use-package org
  :ensure nil  ; Built-in package
  :mode ("\\.org\\'" . org-mode)
  :custom
  ;; Basic org settings
  (org-directory "~/org/")
  (org-default-notes-file (concat org-directory "notes.org"))
  (org-agenda-files (list org-directory))
  (org-return-follows-link t)
  (org-mouse-1-follows-link t)
  (org-link-descriptive t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " ▼")
  
  ;; TODO keywords
  (org-todo-keywords
   '((sequence "TODO(t)" "DOING(o)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-todo-keyword-faces
   '(("TODO" . (:foreground "#ff6c6b" :weight bold))
     ("DOING" . (:foreground "#98be65" :weight bold))
     ("BLOCKED" . (:foreground "#da8548" :weight bold))
     ("DONE" . (:foreground "#46d9ff" :weight bold))
     ("CANCELLED" . (:foreground "#5b6268" :weight bold))))
  
  ;; Agenda settings
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  
  ;; Archive settings
  (org-archive-location "~/org/archive/%s_archive::")
  
  ;; Capture templates
  (org-capture-templates
   '(("t" "Task" entry (file+headline "" "Tasks")
      "* TODO %?\n  %u\n  %a")
     ("j" "Journal" entry (file+datetree "journal.org")
      "* %?\nEntered on %U\n  %i\n  %a")
     ("n" "Note" entry (file "")
      "* %?\n  %u\n  %i\n  %a")
     ("m" "Meeting" entry (file+headline "" "Meetings")
      "* MEETING %?\n  %u\n  %a")))
  
  ;; Export settings
  (org-export-with-smart-quotes t)
  (org-export-with-section-numbers nil)
  (org-export-with-toc nil)
  (org-html-validation-link nil)
  
  ;; Source blocks
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-edit-src-content-indentation 0)
  (org-confirm-babel-evaluate nil)
  
  ;; Performance
  (org-modules nil)  ; Don't load extra modules by default
  (org-startup-folded 'content)
  (org-cycle-separator-lines 2)
  
  :config
  ;; Ensure org directory exists
  (unless (file-directory-p org-directory)
    (make-directory org-directory t))
  
  ;; Enable languages for babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (sql . t)
     (sqlite . t)))
  
  ;; Better org-mode indentation
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'org-mode-hook #'flyspell-mode)
  
  ;; Custom functions
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  
  (add-hook 'org-after-todo-statistics-hook #'org-summary-todo)
  
  ;; Global keybindings
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)))

;; Org-mode specific keybindings using hook
(defun my-org-mode-setup ()
  "Setup keybindings for org-mode."
  (local-set-key (kbd "C-c C-x C-l") 'org-toggle-link-display)
  (local-set-key (kbd "C-c C-x C-v") 'org-toggle-inline-images)
  (local-set-key (kbd "C-c C-x t") 'org-toggle-time-stamp-overlays)
  (local-set-key (kbd "C-c '") 'org-edit-special)
  (local-set-key (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)
  (local-set-key (kbd "C-c C-o") 'org-open-at-point)
  (local-set-key (kbd "M-n") 'org-next-link)
  (local-set-key (kbd "M-p") 'org-previous-link)
  (local-set-key (kbd "C-c |") 'org-table-create-or-convert-from-region))

(add-hook 'org-mode-hook 'my-org-mode-setup)

;; Org-bullets for better visual hierarchy
(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Better org table editing
(use-package org-table
  :ensure nil
  :after org
  :config
  (setq org-table-automatic-realign t))

;; Org-tempo for structure templates (< s TAB, etc.)
(use-package org-tempo
  :ensure nil
  :after org
  :config
  ;; Ensure org-tempo is loaded first
  (require 'org-tempo)
  ;; Add more templates
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("sql" . "src sql")))

;; Basic org-agenda configuration
(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands
        '(("d" "Daily agenda" agenda ""
           ((org-agenda-ndays 3)))
          ("w" "Weekly review" agenda ""
           ((org-agenda-start-day "-7d")
            (org-agenda-span 7))))))