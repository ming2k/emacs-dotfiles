;;; modules/core/completion/org.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Org-mode specific completion configuration
;;; Code:

(defun org-manual-path-completion ()
  "Manual file path completion for org-mode."
  (interactive)
  (let ((completion-at-point-functions '(comint-filename-completion)))
    (completion-at-point)))

;; Disable automatic completion and dabbrev in org-mode to prevent interference
(defun org-disable-completion ()
  "Disable automatic completion and dabbrev in org-mode."
  ;; Disable corfu auto-completion
  (setq-local corfu-auto nil)
  ;; Completely clear completion functions to avoid dabbrev errors
  (setq-local completion-at-point-functions nil))

;; Setup function to be called when org is loaded
(defun setup-org-completion ()
  "Setup org-mode completion behavior."
  (add-hook 'org-mode-hook #'org-disable-completion))

;; Only setup when org is actually loaded
(with-eval-after-load 'org
  (setup-org-completion))

(provide 'org-completion)
;;; modules/core/completion/org.el ends here