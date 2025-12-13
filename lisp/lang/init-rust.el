;;; init-rust.el -*- lexical-binding: t; -*-

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

(provide 'init-rust)