;;; modules/core/completion/markdown-completion.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Markdown specific completion configuration - follows org-mode pattern
;;; Code:

(defun markdown-manual-path-completion ()
  "Manual file path completion for markdown mode."
  (interactive)
  (let ((completion-at-point-functions '(comint-filename-completion)))
    (completion-at-point)))

;; Disable automatic completion and dabbrev in markdown mode to prevent interference
(defun markdown-disable-completion ()
  "Disable automatic completion and dabbrev in markdown mode."
  ;; Disable corfu auto-completion
  (setq-local corfu-auto nil)
  ;; Completely clear completion functions to avoid dabbrev errors
  (setq-local completion-at-point-functions nil))

;; Setup function to be called when markdown-mode is loaded
(defun setup-markdown-completion ()
  "Setup markdown completion behavior."
  (add-hook 'markdown-mode-hook #'markdown-disable-completion)
  (add-hook 'gfm-mode-hook #'markdown-disable-completion))

;; Only setup when markdown-mode is actually loaded
(with-eval-after-load 'markdown-mode
  (setup-markdown-completion))

(provide 'markdown-completion)
;;; modules/core/completion/markdown-completion.el ends here