;;; rust-setup.el -*- lexical-binding: t; -*-

;; Use tree-sitter mode
(add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))

;; Rust mode configuration
(add-hook 'rust-ts-mode-hook
          (lambda ()
            (setq-local rust-ts-mode-indent-offset 4
                        tab-width 4
                        indent-tabs-mode nil)))

;; Enable eglot
(add-hook 'rust-ts-mode-hook #'eglot-ensure)

;; LSP server configuration
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(rust-ts-mode . ("rust-analyzer"))))

(provide 'rust-setup)