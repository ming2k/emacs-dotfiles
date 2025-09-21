;;; config/lang/rust.el -*- lexical-binding: t; -*-

;; Tree-sitter Rust mode (preferred)
(use-package rust-ts-mode
  :ensure nil
  :when (treesit-language-available-p 'rust)
  :mode "\\.rs\\'"
  :hook ((rust-ts-mode . eglot-ensure)
         (rust-ts-mode . flymake-mode))
  :config
  (setq rust-ts-mode-indent-offset 4))

;; Fallback to rust-mode if tree-sitter is not available
(unless (treesit-language-available-p 'rust)
  (use-package rust-mode
    :ensure t
    :mode "\\.rs\\'"
    :hook ((rust-mode . eglot-ensure)
           (rust-mode . flymake-mode))
    :config
    (setq rust-indent-offset 4)))

(provide 'rust-setup)

;;; config/lang/rust.el ends here