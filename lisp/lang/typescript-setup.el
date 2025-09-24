;;; typescript-setup.el -*- lexical-binding: t; -*-

;; Use tree-sitter modes
(add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
(add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))

;; TypeScript mode configuration
(add-hook 'typescript-ts-mode-hook
          (lambda ()
            (setq-local typescript-ts-mode-indent-offset 2
                        tab-width 2
                        indent-tabs-mode nil)))

(add-hook 'tsx-ts-mode-hook
          (lambda ()
            (setq-local typescript-ts-mode-indent-offset 2
                        tab-width 2
                        indent-tabs-mode nil)))

;; Enable eglot
(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
(add-hook 'tsx-ts-mode-hook #'eglot-ensure)

;; LSP server configuration
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode) . ("typescript-language-server" "--stdio"))))

(provide 'typescript-setup)
