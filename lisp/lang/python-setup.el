;;; python-setup.el -*- lexical-binding: t; -*-

;; Use tree-sitter mode
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; Python mode configuration
(add-hook 'python-ts-mode-hook
          (lambda ()
            (setq-local python-indent-offset 4
                        python-shell-interpreter "python3")))

;; Enable eglot for Python
(add-hook 'python-ts-mode-hook #'eglot-ensure)

;; LSP server configuration
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pyright-langserver" "--stdio"))))

(provide 'python-setup)