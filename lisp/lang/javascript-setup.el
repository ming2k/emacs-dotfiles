;;; javascript-setup.el -*- lexical-binding: t; -*-

;; JavaScript mode configuration
(add-hook 'js-ts-mode-hook
          (lambda ()
            (setq-local js-indent-level 2
                        tab-width 2
                        indent-tabs-mode nil)))

;; Enable eglot
(add-hook 'js-ts-mode-hook #'eglot-ensure)

;; LSP server configuration
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(js-ts-mode . ("typescript-language-server" "--stdio"))))

(provide 'javascript-setup)