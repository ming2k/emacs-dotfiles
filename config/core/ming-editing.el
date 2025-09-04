;;; config/core/ming-editing.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Core editing features including desktop session management and fill-column settings
;;; Code:

;; Set default fill column
(setq-default fill-column 80)

;; Fill column indicator and auto-fill mode are NOT enabled globally
;; Let modes decide for themselves based on their needs

;; Custom fill column settings for specific modes
(defun set-fill-column-for-mode (mode column)
  "Set fill-column for a specific MODE to COLUMN value."
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            `(lambda () (setq-local fill-column ,column))))

;; Mode-specific fill column settings
(set-fill-column-for-mode 'emacs-lisp-mode 80)
(set-fill-column-for-mode 'lisp-mode 80)
(set-fill-column-for-mode 'python-mode 88)  ; Black formatter standard
(set-fill-column-for-mode 'rust-mode 100)   ; Rust standard
(set-fill-column-for-mode 'go-mode 100)     ; Go standard

;; Individual modes can enable fill-column-indicator and auto-fill-mode as needed

(provide 'ming-editing)
;;; config/core/ming-editing.el ends here