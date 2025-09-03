;;; config/lang/emacs-lisp-config.el -*- lexical-binding: t; -*-

;;; Emacs Lisp Mode Configuration
(use-package elisp-mode
  :ensure nil
  :hook (emacs-lisp-mode . elisp-setup)
  :config
  (defun elisp-setup ()
    "Basic setup for Emacs Lisp mode."
    ;; Indentation settings
    (setq-local indent-tabs-mode nil)
    (setq-local tab-width 2)
    
    ;; Completion settings - using built-in system
    (setq-local completion-at-point-functions
                '(elisp-completion-at-point))
    
    ;; Parentheses matching
    (show-paren-mode 1)
    
    ;; Disable flyspell
    (flyspell-mode -1)))

;;; Hippie-expand configuration for Emacs Lisp
(defun setup-elisp-hippie-expand ()
  "Setup hippie-expand for Emacs Lisp."
  (setq-local hippie-expand-try-functions-list
              '(try-expand-dabbrev
                try-expand-dabbrev-all-buffers
                try-complete-lisp-symbol
                try-complete-file-name)))

(add-hook 'emacs-lisp-mode-hook #'setup-elisp-hippie-expand)

(provide 'emacs-lisp-config)

;;; config/lang/emacs-lisp-config.el ends here