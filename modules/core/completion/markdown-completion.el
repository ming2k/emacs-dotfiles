;;; modules/core/completion/markdown.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Markdown specific completion configuration
;;; Code:

;; Safe wrapper for dabbrev-capf that handles nil return values
(defun safe-dabbrev-capf ()
  "Safe wrapper for dabbrev-capf that handles nil abbrev cases."
  (condition-case nil
    (when-let ((abbrev (dabbrev--abbrev-at-point)))
      (when (stringp abbrev)
        (dabbrev-capf)))
    (error nil)))

;; Markdown completion setup
(defun markdown-setup-completion ()
  "Setup completion for markdown mode."
  (setq-local completion-at-point-functions
              (list #'safe-dabbrev-capf
                    #'comint-filename-completion)))

;; Setup function to be called when markdown-mode is loaded
(defun setup-markdown-completion ()
  "Setup markdown completion behavior."
  (add-hook 'markdown-mode-hook #'markdown-setup-completion)
  (add-hook 'gfm-mode-hook #'markdown-setup-completion))

;; Only setup when markdown-mode is actually loaded
(with-eval-after-load 'markdown-mode
  (setup-markdown-completion))

(provide 'markdown-completion)
;;; modules/core/completion/markdown.el ends here