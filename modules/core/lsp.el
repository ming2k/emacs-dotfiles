;;; modules/core/lsp.el -*- lexical-binding: t; -*-
;;; Commentary:
;; LSP (Language Server Protocol) configuration using eglot
;;; Code:

;; Eglot configuration - no auto-start hooks (opt-in per language module)
(use-package eglot
  :ensure nil
  :config
  ;; Global eglot settings
  (setq eglot-sync-connect nil
        eglot-autoshutdown t
        eglot-extend-to-xref t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.3
        eglot-ignored-server-capabilities '(:hoverProvider :documentHighlightProvider))
  
  ;; Better eglot keybindings
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format-buffer)
              ("C-c l a" . eglot-code-actions)
              ("C-c l o" . eglot-code-action-organize-imports)
              ("C-c l h" . eldoc)
              ("M-." . eglot-find-declaration)
              ("M-?" . eglot-find-references)
              ("C-M-." . eglot-find-implementation)))

;; Safe wrapper for dabbrev-capf that handles nil return values
(defun safe-dabbrev-capf ()
  "Safe wrapper for dabbrev-capf that handles nil abbrev cases."
  (condition-case nil
    (when-let ((abbrev (dabbrev--abbrev-at-point)))
      (when (stringp abbrev)
        (dabbrev-capf)))
    (error nil)))

;; Programming-specific completion enhancements with corfu
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Prioritize eglot completion for corfu when eglot is managing the buffer
              (setq-local completion-at-point-functions
                          (list #'eglot-completion-at-point
                                #'safe-dabbrev-capf
                                #'comint-filename-completion)))))

(provide 'lsp)
;;; modules/core/lsp.el ends here