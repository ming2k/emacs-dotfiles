;;; config/lang/rust.el -*- lexical-binding: t; -*-

;; Rust tree-sitter mode
(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'"
  :hook ((rust-ts-mode . eglot-ensure))
  :config
  (setq rust-ts-mode-indent-offset 4))

(provide 'rust-setup)

;;; config/lang/rust.el ends here