;;; config/lang/markdown.el -*- lexical-binding: t; -*-
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

;; Setup function to be called when markdown-mode is loaded
(defun setup-markdown-completion ()
  "Setup markdown completion behavior."
  (add-hook 'markdown-mode-hook #'markdown-disable-completion)
  (add-hook 'gfm-mode-hook #'markdown-disable-completion))

;; Markdown mode settings
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :config
  ;; Basic markdown settings
  (setq markdown-command "pandoc"
        markdown-fontify-code-blocks-natively t
        markdown-indent-on-enter nil)
  ;; Setup completion behavior
  (setup-markdown-completion))

(provide 'markdown)
;;; config/lang/markdown.el ends here
