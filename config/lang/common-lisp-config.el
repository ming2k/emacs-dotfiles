;;; config/lang/common-lisp-config.el -*- lexical-binding: t; -*-

;;; Common Lisp Mode Configuration
(use-package lisp-mode
  :ensure nil
  :hook (lisp-mode . lisp-setup)
  :config
  (defun lisp-setup ()
    "Basic setup for Common Lisp mode."
    ;; Indentation settings
    (setq-local indent-tabs-mode nil)
    (setq-local tab-width 2)
    
    ;; Parentheses matching
    (show-paren-mode 1)))

;;; Hippie-expand configuration for Common Lisp
(defun setup-lisp-hippie-expand ()
  "Setup hippie-expand for Common Lisp."
  (setq-local hippie-expand-try-functions-list
              '(try-expand-dabbrev
                try-expand-dabbrev-all-buffers
                try-complete-lisp-symbol
                try-complete-file-name)))

(add-hook 'lisp-mode-hook #'setup-lisp-hippie-expand)

(provide 'common-lisp-config)

;;; config/lang/common-lisp-config.el ends here