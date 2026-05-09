;;; init-markdown.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Simple vanilla Markdown language support with visual line mode
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

;; Setup function to be called when markdown-ts-mode is loaded
(defun setup-markdown-completion ()
  "Setup markdown completion behavior."
  (add-hook 'markdown-ts-mode-hook #'markdown-disable-completion))

;; Markdown mode settings
(use-package markdown-ts-mode
  :ensure nil
  :mode (("\\.md\\'" . markdown-ts-mode)
         ("\\.markdown\\'" . markdown-ts-mode)
         ("README\\.md\\'" . markdown-ts-mode))
  :hook (markdown-ts-mode . eglot-ensure)
  :config
  (setup-markdown-completion))

(provide 'init-markdown)
;;; init-markdown.el ends here
