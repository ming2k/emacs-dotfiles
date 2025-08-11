;;; modules/tools/org/config.el -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :custom
  (org-directory "~/org/")
  (org-default-notes-file (concat org-directory "notes.org"))
  (org-startup-folded 'content)
  (org-pretty-entities t)
  (org-hide-leading-stars t)
  (org-odd-levels-only t)
  (org-todo-keywords '((sequence "TODO(t)" "DOING(g)" "|" "DONE(d!)" "CANCELLED(c!)")))
  (org-log-done 'time)
  (org-agenda-window-setup 'current-window)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-archive-location "archive/%s::datetree/")
  (org-capture-templates
   '(("t" "Task" entry (file+headline org-default-notes-file "Tasks")
      "* TODO %?\n  %U")
     ("j" "Journal" entry (file+datetree org-default-notes-file)
      "* %?\n  %U")))
  :config
  (unless (file-directory-p org-directory)
    (make-directory org-directory t))
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)))
  
  ;; Custom bullet points
  (font-lock-add-keywords 'org-mode
    '(("^\\*\\{1\\} " (0 (prog1 () (compose-region (match-beginning 0) (match-end 0) "•"))))
      ("^\\*\\{2\\} " (0 (prog1 () (compose-region (match-beginning 0) (match-end 0) "  ◦"))))
      ("^\\*\\{3\\} " (0 (prog1 () (compose-region (match-beginning 0) (match-end 0) "    ▪")))))))

;; Disable all completion in org-mode and org-roam
(defun org-disable-all-completion ()
  "Completely disable all completion in org-mode and org-roam."
  ;; Force disable global corfu mode locally
  (when (bound-and-true-p corfu-mode)
    (corfu-mode -1))
  
  ;; Completely clear all completion functions
  (setq-local completion-at-point-functions nil)
  
  ;; Disable all corfu settings
  (setq-local corfu-auto nil)
  (setq-local corfu-auto-delay 999999)
  (setq-local corfu-auto-prefix 999999)
  
  ;; Disable all completion mechanisms
  (setq-local completion-in-region-function nil)
  (setq-local completion-auto-help nil)
  (setq-local tab-always-indent t)
  
  ;; Disable dabbrev completely
  (setq-local dabbrev-case-replace nil)
  (setq-local dabbrev-case-fold-search nil)
  (setq-local dabbrev-abbrev-char-regexp nil)
  
  ;; Disable hippie-expand
  (setq-local hippie-expand-try-functions-list nil)
  
  ;; Remove all potential completion hooks
  (remove-hook 'completion-at-point-functions #'dabbrev-completion t)
  (remove-hook 'completion-at-point-functions #'org-roam-completion-at-point t)
  (remove-hook 'completion-at-point-functions #'org-pcomplete-completion-at-point t)
  (remove-hook 'completion-at-point-functions #'pcomplete-completions-at-point t)
  (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)
  
  ;; Disable org-roam specific completion
  (when (boundp 'org-roam-completion-functions)
    (setq-local org-roam-completion-functions nil)))

(add-hook 'org-mode-hook #'org-disable-all-completion)

;; Aggressively disable completion functions in org-mode
(defun org-disable-completion-advice (orig-fun &rest args)
  "Advice to disable completion functions in org-mode buffers."
  (unless (derived-mode-p 'org-mode)
    (apply orig-fun args)))

;; Add advice to prevent completion functions from running in org-mode
(advice-add 'dabbrev-completion :around #'org-disable-completion-advice)
(advice-add 'completion-at-point :around #'org-disable-completion-advice)
(advice-add 'corfu--auto-complete :around #'org-disable-completion-advice)
(advice-add 'corfu-complete :around #'org-disable-completion-advice)

;; Org-roam (optional)
(use-package org-roam
  :ensure t
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :custom
  (org-roam-directory "~/org/roam/")
  (org-roam-completion-everywhere nil)
  (org-roam-completion-ignore-case t)
  (org-roam-completion-system 'default)
  :config
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  (org-roam-db-autosync-mode)
  (add-hook 'org-roam-mode-hook #'org-disable-all-completion)
  
  ;; Aggressively disable org-roam completion after loading
  (advice-add 'org-roam--register-completion-functions-h :override #'ignore)
  (advice-add 'org-roam-completion-at-point :override #'ignore))

(provide 'org-config)
;;; modules/tools/org/config.el ends here