;;; init-lsp.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Language Server Protocol and diagnostics configuration

;;; Code:

;;; Clear flymake backends in programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local flymake-diagnostic-functions nil)))

;;; Flymake - Error checking (LSP-only)
(use-package flymake
  :ensure nil
  :config
  (setq flymake-no-changes-timeout 0.2
        flymake-start-on-flymake-mode t
        flymake-start-on-save-buffer t)
  (setq-default flymake-diagnostic-functions nil)
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)))

;;; Eglot - LSP client
(use-package eglot
  :ensure nil
  :config
  (setq eglot-sync-connect nil
        eglot-autoshutdown t
        eglot-extend-to-xref t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.1
        eglot-ignored-server-capabilities '(:hoverProvider :documentHighlightProvider))
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c a" . eglot-code-actions)))

;;; LSP integration with flymake
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local flymake-diagnostic-functions nil)
              (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
              (unless flymake-mode (flymake-mode 1))))
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list #'eglot-completion-at-point))))

  ;; LSP server programs for all languages
  (setq eglot-server-programs
        '(;; C/C++
          ((c-ts-mode c++-ts-mode) . ("clangd"))
          ;; Python
          (python-ts-mode . ("pyright-langserver" "--stdio"))
          ;; Rust
          (rust-ts-mode . ("rust-analyzer"))
          ;; JavaScript
          (js-ts-mode . ("typescript-language-server" "--stdio"))
          ;; TypeScript/TSX
          ((typescript-ts-mode tsx-ts-mode) . ("typescript-language-server" "--stdio"))
          ;; Go
          (go-ts-mode . ("gopls"))
          ;; Shell
          ((sh-mode bash-ts-mode) . ("bash-language-server" "start"))
          ;; YAML
          ((yaml-ts-mode yaml-mode) . ("yaml-language-server" "--stdio"))
          ;; JSON
          ((json-ts-mode json-mode) . ("vscode-json-language-server" "--stdio"))
          ;; Markdown
          ((markdown-mode gfm-mode) . ("marksman"))
          ;; Zig
          ((zig-mode zig-ts-mode) . ("zls")))))

(provide 'init-lsp)
;;; init-lsp.el ends here
