;;; modules/core/completion/org.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Org-mode specific completion configuration
;;; Code:

(defun org-manual-path-completion ()
  "Manual file path completion for org-mode."
  (interactive)
  (let ((completion-at-point-functions '(comint-filename-completion)))
    (completion-at-point)))

;; Disable automatic completion but preserve org-mode built-in completion
(defun org-disable-completion ()
  "Disable automatic completion but preserve org-mode built-in functions."
  ;; Disable corfu auto-completion
  (setq-local corfu-auto nil)
  ;; Keep org-mode's built-in completion functions for templates and structure
  (setq-local completion-at-point-functions
              '(org-completion-at-point pcomplete-completions-at-point))
  ;; Ensure structure templates work with TAB
  (setq-local tab-always-indent 'complete))

;; Setup function to be called when org is loaded
(defun setup-org-completion ()
  "Setup org-mode completion behavior."
  (add-hook 'org-mode-hook #'org-disable-completion)
  ;; Ensure structure templates are enabled
  (require 'org-tempo nil t))

;; Only setup when org is actually loaded
(with-eval-after-load 'org
  (setup-org-completion))

(provide 'org-completion)
;;; modules/core/completion/org.el ends here