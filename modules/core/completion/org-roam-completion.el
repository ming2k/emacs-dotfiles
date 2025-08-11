;;; modules/core/completion/org-roam.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Org-roam specific completion configuration
;;; Code:

;; Disable automatic completion and dabbrev in org-roam files
(defun org-roam-disable-completion ()
  "Disable automatic completion and dabbrev in org-roam buffers."
  ;; Disable corfu auto-completion
  (setq-local corfu-auto nil)
  ;; Completely clear completion functions to avoid dabbrev errors
  (setq-local completion-at-point-functions nil))

;; Setup function to be called when org-roam is loaded
(defun setup-org-roam-completion ()
  "Setup org-roam completion behavior."
  ;; Apply completion disabling to org-roam files
  (add-hook 'org-roam-find-file-hook #'org-roam-disable-completion))

;; Only setup when org-roam is actually loaded
(with-eval-after-load 'org-roam
  (setup-org-roam-completion))

(provide 'org-roam-completion)
;;; modules/core/completion/org-roam.el ends here