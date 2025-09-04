;;; config/lang/common-lisp-config.el -*- lexical-binding: t; -*-

;;; Common Lisp Mode Configuration
(use-package lisp-mode
  :ensure nil
  :hook (lisp-mode . lisp-setup)
  :config
  (defun lisp-setup ()
    "Basic setup for Common Lisp mode."
    ;; Fill column settings
    (setq-local fill-column 80)
    
    ;; Indentation settings
    (setq-local indent-tabs-mode nil)
    (setq-local tab-width 2)
    
    ;; Parentheses matching
    (show-paren-mode 1)))

(provide 'lisp-setup)

;;; config/lang/common-lisp-config.el ends here
