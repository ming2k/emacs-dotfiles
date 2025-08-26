;;; modules/lang/lisp/config.el -*- lexical-binding: t; -*-

;;; Common Lisp Mode Configuration
(use-package lisp-mode
  :ensure nil
  :hook ((lisp-mode . lisp-setup)
         (emacs-lisp-mode . lisp-setup))
  :config
  (defun lisp-setup ()
    "Basic setup for Lisp modes."
    ;; Indentation settings
    (setq-local indent-tabs-mode nil)
    (setq-local tab-width 2)
    
    ;; Spell checking for comments and strings
    (flyspell-prog-mode)
    
    ;; Completion settings - using built-in system
    (setq-local completion-at-point-functions
                '(elisp-completion-at-point))
    
    ;; Parentheses matching
    (show-paren-mode 1)
))

;;; Hippie-expand configuration for Lisp
(defun setup-lisp-hippie-expand ()
  "Setup hippie-expand for Lisp."
  (setq-local hippie-expand-try-functions-list
              '(try-expand-dabbrev
                try-expand-dabbrev-all-buffers
                try-complete-lisp-symbol
                try-complete-file-name)))

(add-hook 'lisp-mode-hook #'setup-lisp-hippie-expand)
(add-hook 'emacs-lisp-mode-hook #'setup-lisp-hippie-expand)