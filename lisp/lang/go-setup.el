;;; go-setup.el -*- lexical-binding: t; -*-

;; Use tree-sitter mode
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

;; Go mode configuration
(add-hook 'go-ts-mode-hook
          (lambda ()
            (setq-local tab-width 4
                        indent-tabs-mode t
                        fill-column 100)))

;; Enable eglot
(add-hook 'go-ts-mode-hook #'eglot-ensure)

;; LSP server configuration
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(go-ts-mode . ("gopls"))))

;; Go mod files
(add-to-list 'major-mode-remap-alist '(conf-mode . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-ts-mode))

(provide 'go-setup)
